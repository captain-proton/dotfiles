local wezterm = require 'wezterm'
local mux  = wezterm.mux
local act = wezterm.action

wezterm.on('gui-startup', function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

return {
  color_scheme = 'Nord (base16)',
  font = wezterm.font('JetBrainsMono NF'),
  font_size = 11,
  line_height = 1.3,
  use_dead_keys = false,
  -- How many lines of scrollback you want to retain per tab
  scrollback_lines = 5000,
  enable_scroll_bar = true,

  -- Controls the appearance of the tab bar
  window_frame = {
    font = wezterm.font { family = 'Noto Sans', weight = 'Regular' },
  },

  -- Disable defaults as SUPER key is interfering with the window manager
  disable_default_key_bindings = true,

  -- Adjusting the font size leads to issues when using tiling window managers
  adjust_window_size_when_changing_font_size = false,

  --If set to true, when there is only a single tab, the tab bar is hidden from the display
  hide_tab_bar_if_only_one_tab = true,

  -- timeout_milliseconds defaults to 1000 and can be omitted
  leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 },

  keys = {
    -- Taken of the defaults
    { key = 'Tab', mods = 'CTRL', action = act.ActivateTabRelative(1) },
    { key = 'Tab', mods = 'SHIFT|CTRL', action = act.ActivateTabRelative(-1) },
    { key = 'Enter', mods = 'LEADER', action = act.ActivateCopyMode },
    { key = 'R', mods = 'SHIFT|CTRL', action = act.ReloadConfiguration },
    { key = '+', mods = 'CTRL', action = act.IncreaseFontSize },
    { key = '-', mods = 'CTRL', action = act.DecreaseFontSize },
    { key = '0', mods = 'CTRL', action = act.ResetFontSize },
    { key = 'C', mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
    { key = 'N', mods = 'SHIFT|CTRL', action = act.SpawnWindow },
    { key = 'U', mods = 'SHIFT|CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
    { key = 'V', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
    { key = 'PageUp', mods = 'CTRL', action = act.ActivateTabRelative(-1) },
    { key = 'PageDown', mods = 'CTRL', action = act.ActivateTabRelative(1) },
    { key = 'LeftArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Left' },
    { key = 'RightArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Right' },
    { key = 'UpArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Up' },
    { key = 'DownArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Down' },

    { key = '-', mods = 'LEADER', action = act.SplitVertical { domain = 'CurrentPaneDomain' }, },
    { key = '|', mods = 'LEADER', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' }, },
    { key = 'h', mods = 'LEADER', action = act.ActivatePaneDirection 'Left', },
    { key = 'h', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Left', },
    { key = 'j', mods = 'LEADER', action = act.ActivatePaneDirection 'Down', },
    { key = 'j', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Down', },
    { key = 'k', mods = 'LEADER', action = act.ActivatePaneDirection 'Up', },
    { key = 'k', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Up', },
    { key = 'l', mods = 'LEADER', action = act.ActivatePaneDirection 'Right', },
    { key = 'l', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Right', },
    { key = 'n', mods = 'LEADER', action = act.SpawnTab 'CurrentPaneDomain' },
    { key = 'x', mods = 'LEADER', action = act.CloseCurrentTab{ confirm = true } },
    -- Send "CTRL-B" to the terminal when pressing CTRL-B, CTRL-B
    { key = 'b', mods = 'LEADER|CTRL', action = act.SendString '\x02', },
    { key = 'Enter', mods = 'LEADER', action = act.ActivateCopyMode, },
    { key = 'p', mods = 'LEADER', action = act.PasteFrom 'PrimarySelection' },
    { key = 'k', mods = 'CTRL|ALT', action = act.Multiple
      {
        act.ClearScrollback 'ScrollbackAndViewport',
        act.SendKey { key = 'L', mods = 'CTRL' },
      },
    },
    -- CTRL+b, followed by 'r' will put us in resize-pane
    -- mode until we cancel that mode.
    { key = 'r', mods = 'LEADER', action = act.ActivateKeyTable { name = 'resize_pane', one_shot = false, }, },
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
    resize_pane = {
      { key = 'LeftArrow', action = act.AdjustPaneSize { 'Left', 1 } },
      { key = 'h', action = act.AdjustPaneSize { 'Left', 1 } },

      { key = 'RightArrow', action = act.AdjustPaneSize { 'Right', 1 } },
      { key = 'l', action = act.AdjustPaneSize { 'Right', 1 } },

      { key = 'UpArrow', action = act.AdjustPaneSize { 'Up', 1 } },
      { key = 'k', action = act.AdjustPaneSize { 'Up', 1 } },

      { key = 'DownArrow', action = act.AdjustPaneSize { 'Down', 1 } },
      { key = 'j', action = act.AdjustPaneSize { 'Down', 1 } },

      -- Cancel the mode by pressing escape
      { key = 'Escape', action = 'PopKeyTable' },
    },
  }
}
