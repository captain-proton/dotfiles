local wezterm = require 'wezterm'
local mux  = wezterm.mux
local act = wezterm.action

wezterm.on('gui-startup', function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

return {
  color_scheme = 'Tomorrow Night',
  font = wezterm.font 'JetBrainsMono Nerd Font',
  use_dead_keys = false,
  -- How many lines of scrollback you want to retain per tab
  scrollback_lines = 5000,
  -- Enable the scrollbar.
  -- It will occupy the right window padding space.
  -- If right padding is set to 0 then it will be increased
  -- to a single cell width
  enable_scroll_bar = true,

  -- timeout_milliseconds defaults to 1000 and can be omitted
  leader = { key = 'b', mods = 'CTRL', timeout_milliseconds = 1000 },

  keys = {
    { key = 'S', mods = 'LEADER', action = act.SplitVertical { domain = 'CurrentPaneDomain' }, },
    { key = 'V', mods = 'LEADER', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' }, },
    { key = 'h', mods = 'LEADER', action = act.ActivatePaneDirection 'Left', },
    { key = 'j', mods = 'LEADER', action = act.ActivatePaneDirection 'Down', },
    { key = 'k', mods = 'LEADER', action = act.ActivatePaneDirection 'Up', },
    { key = 'l', mods = 'LEADER', action = act.ActivatePaneDirection 'Right', },
    -- Send "CTRL-B" to the terminal when pressing CTRL-B, CTRL-B
    { key = 'b', mods = 'LEADER|CTRL', action = act.SendString '\x02', },
    { key = 'Enter', mods = 'LEADER', action = act.ActivateCopyMode, },
    { key = 'p', mods = 'LEADER', action = act.PastePrimarySelection, },
    { key = 'K', mods = 'CTRL|SHIFT', action = act.Multiple
      {
        act.ClearScrollback 'ScrollbackAndViewport',
        act.SendKey { key = 'L', mods = 'CTRL' },
      },
    },
  },
}
