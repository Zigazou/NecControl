NecControl
==========

**Warning: I can not guarantee that this program will not cause injury to your monitor, you do it at your own risk**

The `neccontrol` utility allows one to send commands to a Nec Monitor from
the Unix command line.

You can:

- query state of a specific parameter of the monitor,
- change a specific parameter of the monitor.

It works for the following Nec LCD monitors:

- Nec MultiSync P402, P462, P552, P702, 
- Nec MultiSync V322, V421, V422, V423, V462, V463, V551, V552, V651, V652, V801
- Nec MultiSync X401S, X461S, X462S, X462UNV, X463UN, X551S, X551UN, X552S

It currently supports **only TCP/IP connection**, not RS-232C.

Usage
-----

Displays a simple help:

    neccontrol

Display help on all possible options or on a specific option:

    neccontrol help [command]

Get current parameter value from a specific monitor:

    neccontrol <ipAddress> <monitorId> get <command>

Set parameter of a specific monitor:

    neccontrol <ipAddress> <monitorId> set <command> <value>

If an error occured, an error message is sent to the standard error output and
the return code is set to 1.

The output produced by the `get` option can be reused as an input to the `set`
option.

Notes
-----

You should take into account that:

- every name is case sensitive,
- by default, the Nec monitors listen on their serial port, you need to
  configure it to listen to the LAN port (this is mutually exclusive)
- the default IP address is set to 192.168.0.10
- the `get` action waits only for 100 milliseconds for the monitor to answer,
  though it’s lower than the recommended value, it seems to work correctly
- the `set` action waits for 800 milliseconds for the monitor to complete the
  action (the value recommended by Nec is higher than 600 milliseconds), that’s
  why the `set` action may seem slow
- `neccontrol` communicates on port 7142 with the monitor, it is internally set
  because Nec has made this setting not configurable.
- `neccontrol` does not check the validity of:
  - a command for a model of monitor
  - a parameter in a specific context (for example, the V421 monitor cannot do
    PIP with HDMI or DVI as input)

This program is based on information found on the following document:
[External control P.V.X series, NEC LCD Monitor rev. 3.8](http://au.nec.com/en_AU/media/docs/products/displays/External_Control_P.V.X-series.pdf)

Examples
--------

Ask Monitor 1 which input it is currently on:

    neccontrol 192.168.0.10 1 get input # may return vga, rgb-hv, dvi, hdmi…

Ask Monitor 1 to switch on a specific input:

    neccontrol 192.168.0.10 1 set input dvi

Error messages
--------------

Here is a short list of error messages you may encounter:

- **neccontrol: connect: does not exist (No route to host)**

  there is probably no monitor at the specified address

- **Unknown command**

  you have probably mistyped a command name

- **Incorrect value**

  you have probably mistyped a value
 

Available commands
------------------

###Adjust
- **auto-setup** - Auto setup
  * execute (Momentary)
- **h-position** - H position
  * Range: [0..65535] (0=left side, max=right side (depends on display timing))
- **v-position** - V position
  * Range: [0..65535] (0=bottom side, max=top side (depends on display timing))
- **clock** - Clock
  * Range: [0..65535] (display timing)
- **phase** - Phase
  * Range: [0..65535] (display timing)
- **h-resolution** - H resolution
  * Range: [0..65535] (0=low, max=high (display timing))
- **v-resolution** - V resolution
  * Range: [0..65535] (0=low, max=high (display timing))
- **input-resolution** - Input resolution
  * item-1 (always auto)
  * item-2
  * item-3
  * item-4
  * item-5
- **aspect** - Aspect
  * no-operate
  * normal
  * full
  * wide (A/V mode only)
  * zoom
  * dynamic (A/V mode only)
  * off (dot by dot)
- **zoom** - Zoom
  * Range: [1..201] (1=100%, 2=101%, 201=300%)
- **zoom-h-expansion** - Zoom H expansion
  * Range: [1..201] (1=100%, 2=101%, 201=300%)
- **zoom-v-expansion** - Zoom V expansion
  * Range: [1..201] (1=100%, 2=101%, 201=300%)
- **zoom-h-position** - Zoom H position
  * Range: [1..201] (1=100%, 2=101%, 201=300%)
- **zoom-v-position** - Zoom V position
  * Range: [1..201] (1=100%, 2=101%, 201=300%)

###Advanced options
- **input-detect** - Input detect
  * first-detect
  * last-detect
  * none
  * video-detect
  * custom-detect
- **custom-detect-1** - Custom detect (priority 1)
  * vga
  * rgb-hv
  * dvi
  * hdmi
  * video-1
  * video-2
  * s-video
  * dvd-hd1
  * dvd-hd2
  * option
  * display-port
- **custom-detect-2** - Custom detect (priority 2)
  * vga
  * rgb-hv
  * dvi
  * hdmi
  * video-1
  * video-2
  * s-video
  * dvd-hd1
  * dvd-hd2
  * option
  * display-port
- **custom-detect-3** - Custom detect (priority 3)
  * vga
  * rgb-hv
  * dvi
  * hdmi
  * video-1
  * video-2
  * s-video
  * dvd-hd1
  * dvd-hd2
  * option
  * display-port
- **custom-detect-4** - Custom detect (priority 4)
  * vga
  * rgb-hv
  * dvi
  * hdmi
  * video-1
  * video-2
  * s-video
  * dvd-hd1
  * dvd-hd2
  * option
  * display-port
- **custom-detect-5** - Custom detect (priority 5)
  * vga
  * rgb-hv
  * dvi
  * hdmi
  * video-1
  * video-2
  * s-video
  * dvd-hd1
  * dvd-hd2
  * option
  * display-port
- **input-change** - Input change
  * no-operate
  * normal
  * quick
- **dvi-mode** - DVI mode (terminal setting)
  * pc
  * hd
- **bnc-mode** - BNC mode (terminal setting)
  * no-operate
  * rgb
  * component
  * video
  * scart
  * s-video
- **dsub-mode** - D-sub mode (terminal setting)
  * no-operate
  * rgb
  * component
  * video
  * scart
  * s-video
- **hdmi-signal** - HDMI signal (terminal setting)
  * expand
  * raw
- **deinterlace** - Deinterlace
  * off
  * enable
- **color-system** - Color system
  * ntsc
  * pal
  * secam
  * auto
  * ntsc-443 (4.43NTSC)
  * pal-60 (PAL-60)
- **over-scan** - Over scan
  * off
  * on
- **option-setting-audio** - Option setting audio
  * analog
  * digital
- **motion-compensation** - Motion compensation (120Hz)
  * on
  * off

###Audio
- **volume** - Volume
  * Range: [0..100] (0=whisper, 100=loud)
- **balance** - Balance
  * Range: [0..60] (0=left, 30=center, 60=right)
- **treble** - Treble
  * Range: [0..12] (0=min, 6=center, 12=max)
- **bass** - Bass
  * Range: [0..12] (0=min, 6=center, 12=max)
- **pip-audio** - PIP audio
  * no-operate
  * main
  * sub
- **lineout** - Line out
  * no-operate
  * fixed
  * variable
- **surround** - Surround
  * off
  * low
  * high
- **audio-input** - Audio input
  * audio-1 (PC)
  * audio-2
  * audio-3
  * hdmi
  * tv-option
  * display-port

###Carbon footprint
- **carbon-footprint-g** - Readout carbon footprint (g)
  * read-only
- **carbon-footprint-kg** - Readout carbon footprint (kg)
  * read-only
- **carbon-usage-g** - Readout carbon usage (g)
  * read-only
- **carbon-usage-kg** - Readout carbon usage (kg)
  * read-only

###Display protection
- **power-save** - Power save
  * off
  * on
- **video-power-save** - Video power save
  * off
  * on
- **fan-control** - Fan control
  * none
  * auto
  * forced-on
  * auto-2 (offset -2)
  * auto-4 (offset -4)
  * auto-6 (offset -6)
  * auto-8 (offset -8)
  * auto-10 (offset -10)
- **fan-speed** - Fan speed
  * none
  * high
  * low
- **screen-saver-gamma** - Gamma (screen saver)
  * normal
  * screen-saving-gamma
- **screen-saver-brightness** - Brightness (screen saver)
  * normal
  * decrease-brightness
- **screen-saver-motion** - Motion (screen saver)
  * Range: [0..90] (0=off (0s), 90=900s (10s/step))
- **side-border-color** - Side border color
  * Range: [0..100] (0=black, 100=white)
- **auto-brightness** - Auto brightness
  * off
  * on
- **alert-mail** - Alert mail
  * off
  * on

###Multi display
- **monitor-id** - Monitor ID
  * Range: [1..100]
- **group-id** - Group ID
  * Range: [0..1023] (bit 0=Group A, bit 9=Group J)
- **ir-control** - IR control
  * normal
  * primary
  * secondary
  * lock (off)
- **tile-matrix-h-monitor** - H monitor (tile matrix)
  * Range: [1..10] (Number of H-division)
- **tile-matrix-v-monitor** - V monitor (tile matrix)
  * Range: [1..10] (Number of V-division)
- **tile-matrix-position** - Position (tile matrix)
  * Range: [1..100] (1=upper left, max=lower right)
- **tile-comp** - Tile comp (tile matrix)
  * disable (off)
  * enable (on)
- **tile-matrix-mode** - Mode (tile matrix)
  * disable-display (off and display frame)
  * disable-erase (off and erase frame)
  * enable (on)
- **tile-matrix-mem** - Mem (tile matrix)
  * none
  * common (default)
  * each-input
- **power-on-delay** - Power on delay
  * Range: [0..50] (0=off (0s), 50=50s)
- **power-indicator** - Power indicator
  * on
  * off
- **external-control** - External control
  * rs232c
  * lan

###On-Screen Display (OSD)
- **language** - Language (OSD)
  * english
  * german
  * french
  * spanish
  * japanese
  * italian
  * swedish
  * russian
  * chinese
- **menu-display-time** - Menu display time
  * Range: [2..48] (2=10s, 3=15s, 48=240s (5 sec/step))
- **osd-h-position** - OSD H position
  * Range: [0..8192] (0=left, max=right)
- **osd-v-position** - OSD V position
  * Range: [0..8192] (0=down, max=up)
- **osd** - Information OSD
  * disable (disable information OSD)
  * 3s
  * 4s
  * 5s
  * 6s
  * 7s
  * 8s
  * 9s
  * 10s
- **osd-transparency** - OSD transparency
  * opaque
  * translucent
- **osd-rotation** - OSD rotation
  * normal
  * rotated
- **closed-caption** - Closed caption
  * off
  * cc1
  * cc2
  * cc3
  * cc4
  * tt1
  * tt2
  * tt3
  * tt4

###Picture
- **backlight** - Backlight
  * Range: [0..100] (0=dark, 100=bright)
- **contrast** - Contrast
  * Range: [0..100] (0=low, 100=high)
- **sharpness** - Sharpness
  * Range: [0..24] (0=dull, 24=sharp)
- **brightness** - Brightness
  * Range: [0..100] (0=dark, 100=bright)
- **hue** - Hue
  * Range: [0..100] (0=purplish, 100=greenish)
- **color** - Color
  * Range: [0..100] (0=pale, 100=deep)
- **color-temperature** - Color temperature
  * Range: [0..74] (0=2600K, 74=10000K)
- **red** - Color control red
  * Range: [0..200] (0=min, 100=center, 200=max)
- **yellow** - Color control yellow
  * Range: [0..200] (0=min, 100=center, 200=max)
- **green** - Color control green
  * Range: [0..200] (0=min, 100=center, 200=max)
- **cyan** - Color control cyan
  * Range: [0..200] (0=min, 100=center, 200=max)
- **blue** - Color control blue
  * Range: [0..200] (0=min, 100=center, 200=max)
- **magenta** - Color control magenta
  * Range: [0..200] (0=min, 100=center, 200=max)
- **gamma** - Gamma correction
  * native (Native gamma)
  * 2.2 (Gamma 2.2)
  * 2.4 (Gamma 2.4)
  * s (S Gamma)
  * dicom-sim (DICOM SIM)
  * programmable (Programmable)
- **adaptive-contrast** - Adaptive contrast (movie settings)
  * none
  * off
  * low
  * middle
  * high
- **noise-reduction** - Noise reduction (movie settings)
  * off
  * lowest
  * low
  * middle
  * high
  * highest
- **telecine** - Telecine (movie settings)
  * off
  * auto
- **picture-mode** - Picture mode
  * srgb (PC mode only)
  * hi-bright
  * standard
  * cinema (A/V mode only)
  * isf-day (needs an adjustment by ISF)
  * isf-night (needs an adjustment by ISF)
  * ambient-1
  * ambient-2
- **amb-brightness-low** - Ambient brightness low (ambient)
  * Range: [0..100] (0=dark, 100=bright)
- **amb-brightness-high** - Ambient brightness high (ambient)
  * Range: [0..100] (0=dark, 100=bright)
- **illuminance** - Current illuminance (ambient)
  * read-only
- **bright-sensor-read** - Bright sensor read (ambient)
  * read-only

###Picture-In-Picture (PIP)
- **keep-pip-mode** - Keep PIP mode
  * off
  * pip
  * pop
  * still
  * aspect (side by side)
  * full (side by side)
- **pip-size** - PIP size
  * small
  * middle
  * large
- **pip-h-position** - PIP H position
  * Range: [0..100] (0=left, 100=right)
- **pip-v-position** - PIP V position
  * Range: [0..100] (0=top, 100=bottom)
- **pip-aspect** - PIP aspect
  * no-operate
  * normal
  * full
  * wide
- **text-ticker-mode** - Mode (text ticker)
  * none
  * off
  * horizontal
  * vertical
- **text-ticker-position** - Position (text ticker)
  * Range: [0..100] (0=top/left, 100=bottom/right)
- **text-ticker-size** - Size (text ticker)
  * Range: [2..8] (2=narrow(2/24), 8=wide(8/24))
- **text-ticker-blend** - Blend (text ticker)
  * Range: [1..10] (1=10%, 10=100%)
- **text-ticker-detect** - Detect (text ticker)
  * on
  * off
- **text-ticker-fade-in** - Fade in (text ticker)
  * on
  * off
- **pip-input** - PIP input (sub input)
  * vga
  * rgb-hv
  * dvi
  * hdmi
  * video-1
  * video-2
  * s-video
  * dvd-hd1
  * dvd-hd2
  * option
  * display-port

###Scheduling
- **off-timer** - Off timer
  * Range: [0..24] (0=off, 1=1 hour, 24=24 hours)
- **enable-schedule** - Enable schedule
  * Range: [1..7] (1=enable no.1, 7=enable no.7)
- **disable-schedule** - Disable schedule
  * Range: [1..7] (1=disable no.1, 7=disable no.7)

###Settings
- **input** - Input
  * vga
  * rgb-hv
  * dvi
  * hdmi
  * video-1
  * video-2
  * s-video
  * dvd-hd1
  * dvd-hd2
  * option
  * display-port
- **mute** - Mute
  * unmute
  * mute
- **screen-mute** - Screen mute
  * on
  * off
- **mts** - MTS
  * main
  * sub
  * main-sub (Main + sub)
- **still-capture** - Still capture (momentary)
  * off
  * capture
- **signal-information** - Signal information
  * no-action
  * off (no indication)
  * on (indication)
- **tv-channel** - TV channel (up/down)
  * no-action
  * up
  * down

###Temperature sensor
- **select-temperature-sensor** - Select temperature sensor
  * Range: [1..3] (1=sensor #1, 3=sensor #3)
- **temperature** - Readout a temperature
  * read-only
