{ lib
, hasTwm
, socket
,
}: {
  # ET (don't) phone home
  update_check_interval = 0;

  font_family = "Iosevka Extended";
  font_size = lib.mkDefault "16";
  adjust_line_height = "110%";
  # TODO: why?
  box_drawing_scale = "0.001, 1, 1.5, 2";

  #: Cursor customization {{{
  cursor_shape = "beam";
  cursor_beam_thickness = "1.5";
  cursor_underline_thickness = "2.0";
  cursor_blink_interval = "-1";
  # cursor_stop_blinking_after = "15.0";
  #: }}}

  #: Scrollback {{{
  scrollback_lines = "4000";
  scrollback_pager = "less";
  scrollback_pager_history_size = "666"; # in MB
  #: }}}

  #: Window layout {{{
  remember_window_size = false;
  initial_window_width = "640";
  initial_window_height = "800";
  window_padding_width = "10";
  window_margin_width = "0";
  single_window_margin_width = "-1";
  draw_minimal_borders = true;
  hide_window_decorations = hasTwm;
  confirm_os_window_close = "0";
  enabled_layouts = "tall:bias=50;full_size=1;mirrored=false";
  #: }}}

  #: Tab bar {{{
  tab_bar_edge = "bottom";
  tab_bar_style = "fade";
  active_tab_font_style = "bold";
  inactive_tab_font_style = "normal";
  tab_activity_symbol = "߹";
  #: }}}

  #: MacOS {{{
  # Treat option as alt; however can't insert unicode characters with cmd+key.
  macos_option_as_alt = true;
  # Save space by putting the title in the top menubar
  # macos_show_window_title_in = "menubar";
  macos_menubar_title_max_length = 40;
  # Hide the kitty window's title bar on macOS.
  # macos_hide_titlebar = true;
  #: }}}

  #: Advanced {{{
  allow_remote_control = true;
  listen_on = socket;
  # FIXME: why not?
  # startup_session = "session";
  #: }}}

  #: Keyboard shortcuts {{{
  # This is the default value.
  # kitty_mod = "ctrl+shift";
  #: }}}

  # Prevent input latency.
  sync_to_monitor = false;

  enable_audio_bell = true;
}
