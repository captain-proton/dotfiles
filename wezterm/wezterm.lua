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
  key_tables = {
    copy_mode = {
      { key = 'Tab', mods = 'NONE', action = act.CopyMode 'MoveForwardWord', },
      { key = 'Tab', mods = 'SHIFT', action = act.CopyMode 'MoveBackwardWord', },
      { key = 'Space', mods = 'NONE', action = act.CopyMode { SetSelectionMode = 'Cell' }, },
      { key = '^', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLineContent', },
      { key = '$', mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent', },
      { key = '$', mods = 'SHIFT', action = act.CopyMode 'MoveToEndOfLineContent', },
      { key = 'G', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackBottom', },
      { key = 'g', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackTop', },
      { key = 'b', mods = 'NONE', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'w', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
      { key="Escape", mods="NONE", action=act.CopyMode "Close" },
      { key="q", mods = 'NONE', action = act.CopyMode 'Close' },
      { key="h", mods="NONE", action=act.CopyMode "MoveLeft" },
      { key="j", mods="NONE", action=act.CopyMode "MoveDown" },
      { key="k", mods="NONE", action=act.CopyMode "MoveUp" },
      { key="l", mods="NONE", action=act.CopyMode "MoveRight" },
      { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PageUp' },
      { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'PageDown' },
      { key = 'LeftArrow', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
      { key = 'RightArrow', mods = 'NONE', action = act.CopyMode 'MoveRight', },
      { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'MoveUp' },
      { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'MoveDown' },
      -- { key=" ", mods="NONE", action=act.CopyMode "ToggleSelectionByCell" },
      { key="v", mods="NONE", action=act.CopyMode { SetSelectionMode = "Cell" } },
      { key="v", mods="CTRL", action=act.CopyMode { SetSelectionMode = "Block" } },
      -- Enter search mode to edit the pattern.
      -- When the search pattern is an empty string the existing pattern is preserved
      { key="/", mods="SHIFT", action=act.Search { CaseSensitiveString = "" } },
      -- navigate any search mode results
      { key="n", mods="NONE", action=act.CopyMode "NextMatch" },
      { key="N", mods="SHIFT", action=act.CopyMode "PriorMatch" },
      -- Copy/yank to primary selection and leave copy mode
      { key="y", mods="NONE", action=act.Multiple
        {
          act.CopyTo "PrimarySelection",
          act.CopyMode "Close",
        }
      }
    },
    search_mode = {
      { key="Escape", mods="NONE", action=act.CopyMode "Close" },
      -- Go back to copy mode when pressing enter, so that we can use unmodified keys like "n"
      -- to navigate search results without conflicting with typing into the search area.
      { key="Enter", mods="NONE", action=act.ActivateCopyMode },
      { key = 'u', mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
    },
  }
}
