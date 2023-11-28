# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from typing import List
from enum import Enum

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.dgroups import simple_key_binder

MOD = "mod4"
TERMINAL = "wezterm"
BROWSER = "brave"

font_mono = "JetBrainsMono Nerd Font"
font_variable = "Fira Sans"
font_widgets = "CaskaydiaCove Nerd Font"

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([MOD], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([MOD], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([MOD], "j", lazy.layout.down(), desc="Move focus down"),
    Key([MOD], "k", lazy.layout.up(), desc="Move focus up"),
    Key([MOD], "Tab", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key(
        [MOD, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"
    ),
    Key(
        [MOD, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([MOD, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([MOD, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([MOD, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key(
        [MOD, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"
    ),
    Key([MOD, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([MOD, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([MOD], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([MOD], "Return", lazy.spawn(TERMINAL), desc="Launch terminal"),
    Key([MOD], "b", lazy.spawn(BROWSER), desc="Web browser"),
    Key([MOD], "<f2>", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key(
        [MOD],
        "q",
        lazy.spawn("cd ~/.config/rofi/powermenu && ./powermenu.sh"),
        desc="Open the power menu",
    ),
    # Toggle between different layouts as defined below
    Key([MOD, "control"], "k", lazy.next_layout(), desc="Toggle between layouts"),
    Key([MOD, "control"], "j", lazy.previous_layout(), desc="Toggle between layouts"),
    Key([MOD, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([MOD, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([MOD, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
]

group_labels = ("", "", "", "", "󰍺", "󰍡", "󰇮", "󰎄", "󰕧")
group_layouts = ["monadtall" for _ in group_labels]

groups = [
    Group(name=str(i), label=label, layout=group_layouts[i])
    for i, label in enumerate(group_labels)
]

# allow MOD+1 through MOD+0 to bind to groups; if you bind your groups
# by hand in your config, you don't need to do this.
dgroups_key_binder = simple_key_binder(MOD)


def create_move_to_group_key(g: Group, modifiers: List[str] = None) -> Key:
    """Create a Key with which a window can be moved to the given
    group.
    """
    if modifiers is None:
        modifiers = [MOD, "shift"]

    return Key(
        modifiers,
        g.name,
        lazy.window.togroup(g.name, switch_group=False),
        desc=f"Move focused window to group {g.name}",
    )


keys.extend([create_move_to_group_key(g) for g in groups])


class NordColor(Enum):
    POLAR_NIGHT_0 = ["#2E3440", "#2E3440"]
    POLAR_NIGHT_1 = ["#3B4252", "#3B4252"]
    POLAR_NIGHT_2 = ["#434C5E", "#434C5E"]
    POLAR_NIGHT_3 = ["#4C566A", "#4C566A"]
    SNOW_STORM_0 = ["#D8DEE9", "#D8DEE9"]
    SNOW_STORM_1 = ["#E5E9F0", "#E5E9F0"]
    SNOW_STORM_2 = ["#ECEFF4", "#ECEFF4"]
    FROST_0 = ["#8FBCBB", "#8FBCBB"]
    FROST_1 = ["#88C0D0", "#88C0D0"]
    FROST_2 = ["#81A1C1", "#81A1C1"]
    FROST_3 = ["#5E81AC", "#5E81AC"]
    AURORA_RED = ["#BF616A", "#BF616A"]
    AURORA_ORANGE = ["#D08770", "#D08770"]
    AURORA_YELLOW = ["#EBCB8B", "#EBCB8B"]
    AURORA_GREEN = ["#A3BE8C", "#A3BE8C"]
    AURORA_PURPLE = ["#B48EAD", "#B48EAD"]


Nord = [
    ["#2E3440", "#2E3440"],  # bg
    ["#D8DEE9", "#D8DEE9"],  # fg
    ["#3B4252", "#3B4252"],  # color01
    ["#BF616A", "#BF616A"],  # color02
    ["#A3BE8C", "#A3BE8C"],  # color03
    ["#EBCB8B", "#EBCB8B"],  # color04
    ["#81A1C1", "#81A1C1"],  # color05
    ["#B48EAD", "#B48EAD"],  # color06
    ["#88C0D0", "#88C0D0"],  # color07
]

layout_theme = {
    "border_width": 2,
    "margin": 8,
    "border_focus": NordColor.SNOW_STORM_0.value[0],
    "border_normal": NordColor.POLAR_NIGHT_0.value[0],
}
layouts = [
    # layout.MonadWide(**layout_theme),
    # layout.Bsp(**layout_theme),
    # layout.Stack(stacks=2, **layout_theme),
    # layout.Columns(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.Tile(shift_windows=True, **layout_theme),
    # layout.VerticalTile(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.Zoomy(**layout_theme),
    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    layout.Stack(num_stacks=2),
    layout.RatioTile(**layout_theme),
    layout.TreeTab(
        font=font_variable,
        fontsize=11,
        sections=["ONE", "TWO"],
        section_fontsize=10,
        border_width=1,
        bg_color=NordColor.POLAR_NIGHT_0.value,
        active_bg=NordColor.FROST_1.value,
        active_fg=NordColor.POLAR_NIGHT_1.value,
        inactive_bg=NordColor.SNOW_STORM_0.value,
        inactive_fg=NordColor.POLAR_NIGHT_0.value,
        padding_left=0,
        padding_x=0,
        padding_y=5,
        section_top=10,
        section_bottom=20,
        level_shift=8,
        vspace=3,
        panel_width=200,
    ),
    layout.Floating(**layout_theme),
]

widget_defaults = {
    "font": font_widgets,
    "fontsize": 11,
    "padding": 3,
}
extension_defaults = widget_defaults.copy()


spacer_length = 8


def init_main_widgets():
    return [
        widget.Image(
            filename="/usr/share/icons/manjaro/maia/maia.svg",
            scale="False",
            mouse_callbacks={
                "Button1": lambda: lazy.spawn(
                    "rofi -show drun -theme ~/.config/rofi/launcher/launcher.rasi"
                )
            },
        ),
        widget.Prompt(
            font=font_mono, fontsize=12, foreground=NordColor.SNOW_STORM_0.value
        ),
        widget.GroupBox(
            fontsize=11,
            margin_y=3,
            margin_x=4,
            padding_y=2,
            padding_x=3,
            borderwidth=3,
            active=NordColor.FROST_1.value,
            inactive=NordColor.SNOW_STORM_0.value,
            rounded=False,
            highlight_color=NordColor.POLAR_NIGHT_1.value,
            highlight_method="line",
            this_current_screen_border=NordColor.FROST_3.value,
            this_screen_border=NordColor.AURORA_GREEN.value,
            other_current_screen_border=NordColor.FROST_3.value,
            other_screen_border=NordColor.AURORA_GREEN.value,
        ),
        widget.CurrentLayoutIcon(
            foreground=NordColor.SNOW_STORM_0.value,
            padding=0,
            scale=0.7,
        ),
        widget.CurrentLayout(foreground=NordColor.SNOW_STORM_0.value, padding=5),
        widget.TextBox(
            text=" ∣ ",
            font=font_mono,
            foreground=NordColor.SNOW_STORM_0.value,
            padding=2,
            fontsize=14,
        ),
        widget.WindowName(foreground=NordColor.SNOW_STORM_0.value, max_chars=60),
        # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
        # widget.StatusNotifier(),
        # aur-updates
        widget.CheckUpdates(
            distro="Arch_yay",
            colour_have_updates=NordColor.AURORA_GREEN.value,
            colour_no_updates=NordColor.POLAR_NIGHT_3.value,
        ),
        # arch-updates
        widget.CheckUpdates(
            distro="Arch",
            colour_have_updates=NordColor.AURORA_GREEN.value,
            colour_no_updates=NordColor.POLAR_NIGHT_3.value,
        ),
        # battery
        # widget.Battery(
        # ),
        # temperature
        widget.ThermalSensor(format="{tag}: {temp:.0f}{unit}"),
        # cpu
        widget.CPU(
            format="󰘚 {load_percent}%",
            foreground=NordColor.AURORA_GREEN.value,
        ),
        # mem
        widget.Spacer(length=spacer_length),
        widget.Memory(
            foreground=NordColor.FROST_1.value,
            mouse_callbacks={"Button1": lambda: lazy.spawn(MOD + " -e btm")},
            format="{MemUsed: .0f}{mm}",
            fmt="󰍛 {}",
        ),
        # pulseaudio-control
        # bluetooth
        # date
        # powermenu
        # spacer
    ]


screens = [
    Screen(
        top=bar.Bar(
            widgets=init_main_widgets(),
            size=24,
            border_width=[1 for _ in range(4)],  # Draw borders around
            border_color=[
                NordColor.SNOW_STORM_0.value for _ in range(4)
            ],  # Borders are fg
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [MOD],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [MOD], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([MOD], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
