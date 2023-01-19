-- Load the Hammerspoon CLI tools.
local ipc = require("hs.ipc")
if not ipc.cliStatus("/usr/local") then
  ipc.cliInstall()
end

-- hs.hotkey.bindSpec({command, "e"},
--   function ()
--     hs.task.new("zsh", nil, { "-l", "-c", "emacsclient --eval '(emacs-everywhere)'" }):start()
--   end
-- )

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "W", function()
  hs.alert.show("Hello World!")
end)
