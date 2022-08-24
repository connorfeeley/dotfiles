-- Load the Hammerspoon CLI tools.
local ipc = require("hs.ipc")
if not ipc.cliStatus("/usr/local") then
  ipc.cliInstall()
end

hs.hotkey.bindSpec({command, "e"},
  function ()
    hs.task.new("zsh", nil, { "-l", "-c", "emacsclient --eval '(emacs-everywhere)'" }):start()
  end
)
