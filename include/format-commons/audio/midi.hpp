/*
 * Copyright 2020 Fabian Stiewitz <fabian@stiewitz.pw>
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
#ifndef FORMAT_COMMONS_AUDIO_MIDI_HPP
#define FORMAT_COMMONS_AUDIO_MIDI_HPP

#include <format.hpp>
#include <utility>

#include <format-commons/audio/x-midi.hpp>

namespace format::audio::midi {

    enum Variables {
        CHUNK_TYPE = 32,
        CHUNK_LENGTH
    };

    struct midi_format_exception : public std::runtime_error {
        explicit midi_format_exception(const std::string &e) : std::runtime_error(e) {}
    };

    struct header_t {
        enum format_t : uint16_t {
            MULTI_CHANNEL, SIMULTRACK, INDEPENDENT
        };
        format_t format;
        uint16_t ntrks;
        struct metrical_time_t {
            uint16_t ticks_per_quarter_note;
        };
        struct time_code_time_t {
            int8_t smpte_format;
            uint8_t ticks_per_frame;
        };
        struct division_t {
            enum {
                METRICAL, TIME_CODE_BASED
            } format;
            std::variant<metrical_time_t, time_code_time_t> data;
        };
        division_t division;

        header_t(uint16_t format, uint16_t ntrks, division_t division) : format(static_cast<format_t>(format)),
                                                                         ntrks(ntrks), division(std::move(division)) {
            if (format > 2u) throw midi_format_exception("invalid MIDI format value");
            if (format == 0 && ntrks != 1)
                throw midi_format_exception("format 0 file needs to have exactly 1 track chunk");
            if (division.format == division_t::TIME_CODE_BASED) {
                auto v = std::get<time_code_time_t>(division.data).smpte_format;
                if (v != -24 && v != -25 && v != -29 && v != -30) throw midi_format_exception("invalid SMPTE format");
            }
        }
    };

    header_t::division_t read_division(uint16_t in) {
        if (in & 0x8000u) {
            uint8_t vp = (in & 0x7f00u) >> 8u;
            auto v = *reinterpret_cast<int8_t *>(&vp);
            return {header_t::division_t::TIME_CODE_BASED,
                    header_t::time_code_time_t{v,
                                               static_cast<uint8_t>(in & 0xffu)}};
        } else {
            return {header_t::division_t::METRICAL, header_t::metrical_time_t{in}};
        }
    }

    uint16_t write_division(const header_t::division_t &t) {
        uint8_t v;
        switch (t.format) {
            case header_t::division_t::METRICAL:
                return std::get<header_t::metrical_time_t>(t.data).ticks_per_quarter_note;
            case header_t::division_t::TIME_CODE_BASED:
                return 0x8000u |
                       *reinterpret_cast<const uint8_t *>(&std::get<header_t::time_code_time_t>(t.data).smpte_format)
                               << 8u |
                       std::get<header_t::time_code_time_t>(t.data).ticks_per_frame;
        }
        return 0;
    }

    struct HeaderType {
        static constexpr const char *value = "MThd";
    };
    struct TrackType {
        static constexpr const char *value = "MTrk";
    };

    using HeaderChunk = Structure <header_t,
    O<offsetof(header_t, format), Sc < uint16_t, BigEndian>>,
    O<offsetof(header_t, ntrks), Sc<uint16_t, BigEndian>>,
    O<offsetof(header_t, division), Map<read_division, write_division, Sc < uint16_t, BigEndian>>>>;

    struct event_t {
        uint32_t delta;
        x_midi::midi_message_t message;

        template<typename T>
        event_t(uint32_t d, T &&msg): delta(d), message(std::forward<T>(msg)) {}
    };

    using Event = Structure<event_t,
    O<offsetof(event_t, delta), x_midi::VariableLength>,
    O<offsetof(event_t, message), x_midi::MidiMessageWithMeta>>;

    template<typename T, typename L>
    struct FixedByteArray {
        FORMAT_HPP_TYPE(T)

        FORMAT_HPP_INTERFACE_DECL
        {
            FORMAT_HPP_INTERFACE
            using Type = std::vector<typename T::template Interface<S>::Type>;

            void read() {
                auto l = stack.template get<L::ID>().val;
                if constexpr (std::is_signed_v<typeof(l)>) {
                    if (l < 0) throw midi_format_exception("chunk length cannot be less than zero");
                }
                processor.seekg(processor.cur, l);
            }

            void read(Type &output) {
                output.clear();
                auto l = stack.template get<L::ID>().val;
                auto s = processor.tellg();
                auto c = 0u;
                while (c < l) {
                    x_midi::midi_message_t message;
                    uint32_t delta;
                    Format<x_midi::VariableLength, x_midi::MidiMessageWithMeta>::reader(processor).read(delta).read(message);
                    c = (processor.tellg() - s);
                    output.emplace_back(delta, std::move(message));
                }
            }

            void write(const Type &input) {
                for (auto &t : input) {
                    Format<x_midi::VariableLength, x_midi::MidiMessageWithMeta>::writer(processor).write(t.delta).write(
                            t.message);
                }
            }
        };
    };

    struct meta_event_sequence_number_t : public x_midi::meta_event_t {
        meta_event_sequence_number_t(uint16_t seqno) : meta_event_t(0, 2, reinterpret_cast<char *>(&seqno)) {}
    };

    template<int Type>
    struct meta_event_text_event_t : public x_midi::meta_event_t {
        meta_event_text_event_t(std::string s) : meta_event_t(Type, s.size(), s.data()) {}
    };

    using meta_event_copyright_notice_t = meta_event_text_event_t<2>;
    using meta_event_sequence_name_t = meta_event_text_event_t<3>;
    using meta_event_instrument_name_t = meta_event_text_event_t<4>;
    using meta_event_lyric_t = meta_event_text_event_t<5>;
    using meta_event_marker_t = meta_event_text_event_t<6>;
    using meta_event_cue_point_t = meta_event_text_event_t<7>;

    struct meta_event_channel_prefix_t : public x_midi::meta_event_t {
        meta_event_channel_prefix_t(uint8_t x) : meta_event_t(0x20, 1, reinterpret_cast<char *>(&x)) {
            if (x >= 16) throw midi_format_exception("MIDI channels have to be between 0 and 15");
        }
    };

    struct meta_event_end_of_track_t : public x_midi::meta_event_t {
        meta_event_end_of_track_t() : meta_event_t(0x2f, 0, nullptr) {}
    };

    struct meta_event_set_tempo_t : public x_midi::meta_event_t {
        meta_event_set_tempo_t(uint32_t x) : meta_event_t(0x51, 3, nullptr) {
            bytes[0] = 0xffu & (x >> 16u);
            bytes[1] = 0xffu & (x >> 8u);
            bytes[2] = 0xffu & x;
        }
    };

    struct meta_event_smpte_offset_t : public x_midi::meta_event_t {
        meta_event_smpte_offset_t(uint8_t hr, uint8_t mn, uint8_t se, uint8_t fr, uint8_t ff) : meta_event_t(0x54, 5,
                                                                                                             nullptr) {
            std::array<uint8_t, 5> v{};
            v[0] = hr;
            v[1] = mn;
            v[2] = se;
            v[3] = fr;
            v[4] = ff;
            memcpy(bytes.data(), v.data(), 5);
        }
    };

    struct meta_event_time_signature_t : public x_midi::meta_event_t {
        meta_event_time_signature_t(uint8_t nn, uint8_t dd, uint8_t cc, uint8_t bb): meta_event_t(0x58, 4, nullptr) {
            std::array<uint8_t, 4> v{};
            v[0] = nn;
            v[1] = dd;
            v[2] = cc;
            v[3] = bb;
            memcpy(bytes.data(), v.data(), 4);
        }
    };

    struct meta_event_key_signature_t : public x_midi::meta_event_t {
        meta_event_key_signature_t(uint8_t sf, uint8_t mi): meta_event_t(0x59, 2, nullptr) {
            std::array<uint8_t, 2> v{};
            v[0] = sf;
            v[1] = mi;
            memcpy(bytes.data(), v.data(), 2);
        }
    };

    using TrackChunk = FixedByteArray<Event, Get < CHUNK_LENGTH>>;

    using ChunkSub = Switch <Get<CHUNK_TYPE>,
    Case<Constant < HeaderType>, HeaderChunk>,
    Case <Constant<TrackType>, TrackChunk>>;

    struct chunk_t {
        std::string type;
        uint32_t length;
        std::variant<header_t, std::vector<event_t>> data;
    };

    using Chunk = Structure <chunk_t, O<offsetof(chunk_t, type), Copy < CHUNK_TYPE, StaticString < 4>>>,
    O<offsetof(chunk_t, length), Copy<CHUNK_LENGTH, Sc < uint32_t, BigEndian>>>,
    O<offsetof(chunk_t, data), ChunkSub>>;

}

#endif //FORMAT_COMMONS_AUDIO_MIDI_HPP
