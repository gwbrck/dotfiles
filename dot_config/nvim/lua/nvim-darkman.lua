local M = {}
local loop = vim.loop
local api = vim.api
local ldbus = require 'ldbus'
local conn = assert ( ldbus.bus.get("session") )

M.init = function()

   -- Create a message for the method call we want to make.

   local msg = assert ( ldbus.message.new_method_call (
                           "org.freedesktop.impl.portal.desktop.darkman",
                           "/org/freedesktop/portal/desktop",
                           "org.freedesktop.impl.portal.Settings" ,
                           "Read" ) , "Message Null" )


   local iter = ldbus.message.iter.new ( )
   msg:iter_init_append ( iter )
   
   -- assert ( iter:append_basic ( {"org.freedesktop.appearance", "color-scheme"} ) , "Out of Memory" )
   iter:append_basic("org.freedesktop.appearance")
   iter:append_basic("color-scheme")


   -- Send the message
   local reply = assert ( conn:send_with_reply_and_block ( msg ) )

   -- Retrieve the result returned (reusing the iterator).
   assert ( reply:iter_init ( iter ) , "Message has no arguments" )

   -- The returned result in this case is of the type "variant" so after a bit
   -- of head scratching I figured out you need to recurse into it, so create a
   -- sub iterator to do that.
   local subiter = ldbus.message.iter.new ( )
   iter:recurse(subiter)

   -- Temperature value returned is of type double and is in kelvin, so
   -- converting to Celsius and make it an integer.

   local theme_value = subiter:get_basic()
   if theme_value == 2 then
      vim.cmd([[colorscheme edge]])
      vim.o.background = "light"
   elseif theme_value == 1 then
      vim.cmd([[colorscheme edge]])
      vim.o.background = "dark"
   end
   return
end

   


M.init()



return M
