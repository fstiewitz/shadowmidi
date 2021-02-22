## shadowmidi
Automatic ALSA-Midi Recorder

### Usage
This command-line tool continuously records MIDI-Events
from an ALSA Midi input and automatically saves them (after n minutes of silence) as Format 0 midi files.

```
shadowmidi CLIENT PORT BREAK OUT
```

`CLIENT` and `PORT` address the ALSA midi port. After `BREAK` minutes a midi recording is considered finished and saved
as `OUT/YYYY-MM-DD hh.mm.ss.midi`.

### Building

Requires ALSA-Headers and a C++17-Compiler.

```
mkdir build
cd build
cmake ..
make
sudo make install
```

### Limitations
Because I don't need it.

- Channel information is not preserved (program assumes single channel input)
- Only notes, control and program changes are recorded

### License

LGPLv3+
