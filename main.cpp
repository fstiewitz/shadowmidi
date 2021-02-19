/*
 * Copyright 2021 Fabian Stiewitz <fabian@stiewitz.pw>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <iostream>
#include <memory>
#include <fstream>
#include <sstream>

#include <csignal>

#include <alsa/asoundlib.h>
#include <alsa/seq.h>

#include <format-commons/audio/x-midi.hpp>
#include <format-commons/audio/midi.hpp>

using namespace format::audio::x_midi;
using namespace format::audio::midi;

#define UNIQ(x, y, name) std::unique_ptr<x, decltype(&y)>name(nullptr, y);

template<typename T, typename D>
class Unique {
    T _comp;
    D _del;
public:
    Unique(T &&c, D &&d) : _comp(std::forward<T>(c)), _del(std::forward<D>(d)) {}

    virtual ~Unique() {
        _del(std::move(_comp));
    }

    auto &get() {
        return _comp;
    }

    [[nodiscard]] auto &get() const {
        return _comp;
    }

    template<typename T2=T, std::enable_if_t<!std::is_class_v<T2>, int> = 0>
    explicit operator T() {
        return _comp;
    }
};

bool terminate = false;
bool should_save = false;

void oninterrupt(int h) {
    terminate = true;
}

void onusr1(int h) {
    should_save = true;
}

using MessageVector = std::vector<event_t>;

void savemessages(MessageVector &messages, int ppq) {
    if (messages.size() <= 1) return;
    // null time
    messages.front().delta = 0;
    // end of track
    messages.emplace_back(messages.back().delta + 1, midi_message_t(0xff, system_message_t(meta_event_end_of_track_t())));
    // prepare output data
    header_t::division_t division{
            header_t::division_t::METRICAL,
            header_t::metrical_time_t{static_cast<uint16_t>(ppq)}
    };
    chunk_t header{
            HeaderType::value,
            6,
            header_t{
                    header_t::MULTI_CHANNEL,
                    1,
                    header_t::division_t{
                            header_t::division_t::METRICAL,
                            header_t::metrical_time_t{
                                static_cast<uint16_t>(ppq)
                            }
                    }
            }
    };
    // get length of track in bytes
    chunk_t track{
            TrackType::value,
            0,
            messages
    };
    {
        std::stringstream sf{};
        format::Format<Chunk>::writer(sf).write(track);
        track.length = sf.str().size() - strlen(TrackType::value) - 4;
    }
    // output
    std::ofstream of("test.midi", std::ios_base::binary | std::ios_base::out);
    format::Format<Chunk>::writer(of).write(header);
    format::Format<Chunk>::writer(of).write(track);
    of.close();
    fprintf(stderr, "saved MIDI output\n");
}

void save(MessageVector &messages, int ppq, long tick, long vtempo) {
    savemessages(messages, ppq);
    messages.clear();
    messages.emplace_back(tick, midi_message_t(0xff, meta_event_set_tempo_t(vtempo)));
}

int main(int argc, char **argv) {
    struct sigaction intaction{};
    struct sigaction usr1action{};
    memset(&intaction, 0, sizeof(struct sigaction));
    memset(&usr1action, 0, sizeof(struct sigaction));
    sigemptyset(&intaction.sa_mask);
    sigemptyset(&usr1action.sa_mask);
    intaction.sa_handler = oninterrupt;
    intaction.sa_flags = 0;
    usr1action.sa_handler = onusr1;
    usr1action.sa_flags = 0;
    if (sigaction(SIGINT, &intaction, nullptr) == -1) {
        perror("sigaction SIGINT");
        return 1;
    }
    if (sigaction(SIGTERM, &intaction, nullptr) == -1) {
        perror("sigaction SIGTERM");
        return 1;
    }
    if (sigaction(SIGUSR1, &usr1action, nullptr) == -1) {
        perror("sigaction SIGUSR1");
        return 1;
    }

    if (argc != 4) {
        fprintf(stderr, "Usage: shadowmidi CLIENT PORT BREAK\n");
        return 1;
    }
    int clientno = int(strtol(argv[1], nullptr, 10));
    int portno = int(strtol(argv[2], nullptr, 10));
    int min_break_time = int(strtol(argv[3], nullptr, 10));
    if (errno) {
        fprintf(stderr, "Could not read input arguments\n");
        return 1;
    }

    UNIQ(snd_seq_t, snd_seq_close, client);
    snd_seq_t *handle;
    int err = snd_seq_open(&handle, "default", SND_SEQ_OPEN_DUPLEX, 0);
    if (err < 0) {
        fprintf(stderr, "could not open ALSA\n");
        return 1;
    }
    client.reset(handle);
    auto ownclientno = snd_seq_client_id(client.get());
    snd_seq_set_client_name(client.get(), "ShadowMIDI");

    Unique queue(
            snd_seq_alloc_queue(client.get()),
            [&client](int q) {
                snd_seq_free_queue(client.get(), q);
                fprintf(stderr, "freed queue\n");
            }
    );
    if (int(queue) < 0) {
        fprintf(stderr, "could not create ALSA input queue\n");
        return 1;
    }

    snd_seq_queue_tempo_t *tempo;
    snd_seq_queue_tempo_alloca(&tempo);
    int bpm = 100;
    int ppq = 48;
    long vtempo = 60000000l / bpm;
    snd_seq_queue_tempo_set_tempo(tempo, 60000000l / bpm);
    snd_seq_queue_tempo_set_ppq(tempo, ppq);
    err = snd_seq_set_queue_tempo(client.get(), int(queue), tempo);
    if (err < 0) {
        fprintf(stderr, "cannot set ALSA queue tempo\n");
        return 1;
    }

    err = snd_seq_start_queue(client.get(), int(queue), nullptr);
    if (err < 0) {
        fprintf(stderr, "could not start ALSA queue (%s)\n", snd_strerror(err));
        return 1;
    }

    snd_seq_port_info_t *port_info;
    snd_seq_port_info_alloca(&port_info);
    snd_seq_port_info_set_capability(port_info, SND_SEQ_PORT_CAP_WRITE | SND_SEQ_PORT_CAP_SUBS_WRITE);
    snd_seq_port_info_set_type(port_info, SND_SEQ_PORT_TYPE_MIDI_GENERIC);
    snd_seq_port_info_set_name(port_info, "MIDI in");
    snd_seq_port_info_set_timestamping(port_info, 1);
    snd_seq_port_info_set_timestamp_real(port_info, 0);
    snd_seq_port_info_set_timestamp_queue(port_info, int(queue));

    Unique port(
            snd_seq_create_port(client.get(), port_info),
            [&client](int p) {
                snd_seq_delete_port(client.get(), p);
                fprintf(stderr, "deleted port\n");
            }
    );
    if (int(port) < 0) {
        fprintf(stderr, "could not create ALSA input port\n");
        return 1;
    }

    snd_seq_drain_output(client.get());

    snd_seq_addr_t sender, dest;
    snd_seq_port_subscribe_t *subs;
    sender.client = clientno;
    sender.port = portno;
    dest.client = ownclientno;
    dest.port = int(port);
    snd_seq_port_subscribe_alloca(&subs);
    snd_seq_port_subscribe_set_sender(subs, &sender);
    snd_seq_port_subscribe_set_dest(subs, &dest);
    snd_seq_port_subscribe_set_queue(subs, int(queue));
    snd_seq_port_subscribe_set_time_update(subs, 1);
    snd_seq_port_subscribe_set_time_real(subs, 1);
    err = snd_seq_subscribe_port(client.get(), subs);
    if (err < 0) {
        fprintf(stderr, "could not connect to ALSA device\n");
        return 1;
    }

    Unique connection(
            (snd_seq_port_subscribe_t *) subs,
            [&client](auto n) {
                snd_seq_unsubscribe_port(client.get(), n);
                fprintf(stderr, "disconnected client\n");
            }
    );

    snd_seq_queue_status_t *queue_status;
    snd_seq_queue_status_alloca(&queue_status);

    MessageVector messages{};
    messages.emplace_back(0, midi_message_t(0xff, meta_event_set_tempo_t(vtempo)));

    long last_tick = 0;

    while (!terminate) {
        snd_seq_event_t *ev;
        auto size = snd_seq_event_input(client.get(), &ev);
        if (size == -ENOSPC) continue;
        if (size == -EINTR) {
            if(terminate) break;
            continue;
        }
        if (size < 0) {
            fprintf(stderr, "ALSA event has unexpected return value (%s)\n", snd_strerror(size));
            break;
        }
        if (!snd_seq_ev_is_tick(ev)) continue;
        // check for break
        if (!messages.empty()) {
            auto tickSec = 100.0 / (bpm * ppq);
            auto tick = ev->time.tick - last_tick;
            auto delta = tickSec * double(tick) / 60;
            if (delta > min_break_time || should_save) {
                should_save = false;
                last_tick = ev->time.tick;
                save(messages, ppq, tick, vtempo);
            }
        }
        // process message
        bool process_msg = false;
        midi_message_t msg{};
        switch (ev->type) {
            case SND_SEQ_EVENT_NOTEON:
                process_msg = true;
                if (ev->data.note.velocity) {
                    fprintf(stderr, "note on\n");
                    msg = midi_message_t(make_status_byte(NOTEON, ev->data.note.channel),
                                         note_on_t(ev->data.note.note, ev->data.note.velocity));
                    break;
                }
            case SND_SEQ_EVENT_NOTEOFF:
                process_msg = true;
                fprintf(stderr, "note off\n");
                msg = midi_message_t(make_status_byte(NOTEOFF, ev->data.note.channel),
                                     note_off_t(ev->data.note.note, ev->data.note.velocity));
                break;
            case SND_SEQ_EVENT_CONTROLLER:
                process_msg = true;
                fprintf(stderr, "controller\n");
                msg = midi_message_t(make_status_byte(CONTROLCHANGE, ev->data.control.channel),
                                     control_change_t(ev->data.control.param, ev->data.control.value));
                break;
            case SND_SEQ_EVENT_PGMCHANGE:
                process_msg = true;
                fprintf(stderr, "program\n");
                msg = midi_message_t(make_status_byte(PROGRAMCHANGE, ev->data.control.channel),
                                     program_change_t(ev->data.control.value));
                break;
            case SND_SEQ_EVENT_CHANPRESS:
                process_msg = true;
                fprintf(stderr, "channel pressure\n");
                msg = midi_message_t(make_status_byte(CHANNELPRESSURE, ev->data.control.channel),
                                     channel_pressure_t(ev->data.control.value));
                break;
            case SND_SEQ_EVENT_PITCHBEND:
                process_msg = true;
                fprintf(stderr, "pitch wheel\n");
                msg = midi_message_t(make_status_byte(PITCHWHEELCHANGE, ev->data.control.channel),
                                     pitch_wheel_change_t(ev->data.control.value));
                break;
            case SND_SEQ_EVENT_SONGPOS:
                process_msg = true;
                fprintf(stderr, "song pos\n");
                msg = midi_message_t(make_status_byte(SONG_POSITION_POINTER, ev->data.control.channel),
                                     song_position_pointer_t(ev->data.control.value));
                break;
            case SND_SEQ_EVENT_SONGSEL:
                process_msg = true;
                fprintf(stderr, "song pos\n");
                msg = midi_message_t(make_status_byte(SONG_SELECT, ev->data.control.channel),
                                     song_select_t(ev->data.control.value));
                break;
        }
        if (!process_msg) continue;
        auto tick = ev->time.tick - last_tick;
        last_tick = ev->time.tick;
        messages.emplace_back(tick, msg);
    }
    if(!messages.empty()) {
        savemessages(messages, ppq);
    }

    return 0;
}
