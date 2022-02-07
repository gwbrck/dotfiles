local vim = vim
local lsp_status = require('lsp-status')
local devicons = require'nvim-web-devicons'
local Job = require('plenary.job')
local curr_files = {}
local blank = ' '
local purple = vim.api.nvim_eval("synIDattr(synIDtrans(hlID('Purple')), 'fg', 'gui')")
local blue = vim.api.nvim_eval("synIDattr(synIDtrans(hlID('Blue')), 'fg', 'gui')")
local yellow = vim.api.nvim_eval("synIDattr(synIDtrans(hlID('Yellow')), 'fg', 'gui')")
local green = vim.api.nvim_eval("synIDattr(synIDtrans(hlID('Green')), 'fg', 'gui')")
local red = vim.api.nvim_eval("synIDattr(synIDtrans(hlID('Red')), 'fg', 'gui')")
local bg_color = vim.api.nvim_eval("synIDattr(synIDtrans(hlID('StatusLine')), 'bg', 'gui')")
local fg_color = vim.api.nvim_eval("synIDattr(synIDtrans(hlID('StatusLine')), 'bg', 'gui')")
local fg_colorNC = vim.api.nvim_eval("synIDattr(synIDtrans(hlID('StatusLineNC')), 'fg', 'gui')")
vim.cmd('hi! StatusLineNumbers guibg=' .. bg_color .. ' guifg=' .. fg_colorNC)

-- Mode Prompt Table
local current_mode = setmetatable({
      ['n'] = ' N ',
      ['no'] = ' N·Operator Pending ',
      ['v'] = ' V ',
      ['V'] = ' V·Line ',
      ['^V'] = ' V·Block ',
      ['s'] = ' Select ',
      ['S'] = ' S·Line ',
      ['^S'] = ' S·Block ',
      ['i'] = ' I ',
      ['ic'] = ' I ',
      ['ix'] = ' I ',
      ['R'] = ' R ',
      ['Rv'] = ' V·Replace ',
      ['c'] = ' COMMAND ',
      ['cv'] = ' Vim Ex ',
      ['ce'] = ' Ex ',
      ['r'] = ' Prompt ',
      ['rm'] = ' More ',
      ['r?'] = ' Confirm ',
      ['!'] = ' Shell ',
      ['t'] = ' TERM '
    }, {
      -- fix weird issues
      __index = function(_, _)
        return ' V·Block '
      end
    }
)

local RedrawColors = function(mode)
  if mode == 'n' then
    vim.api.nvim_command('hi Mode guibg='..green..' guifg='..fg_color..' gui=bold')
  end
  if mode == 'i' then
    vim.api.nvim_command('hi Mode guibg='..blue..' guifg='..fg_color..' gui=bold')
  end
  if mode == 'v' or mode == 'V' or mode == '^V' then
    vim.api.nvim_command('hi Mode guibg='..purple..' guifg='..fg_color..' gui=bold')
  end
  if mode == 'c' then
    vim.api.nvim_command('hi Mode guibg='..yellow..' guifg='..fg_color..' gui=bold')
  end
  if mode == 't' then
    vim.api.nvim_command('hi Mode guibg='..red..' guifg='..fg_color..' gui=bold')
  end
end

local function file_icon()
    local f_name,f_extension = vim.fn.expand('%:t'),vim.fn.expand('%:e')
    local icon, icon_highlight = devicons.get_icon(f_name,f_extension, { default = true })
    local fg_color_icon = vim.api.nvim_eval("synIDattr(synIDtrans(hlID('" .. icon_highlight .. "')), 'fg', 'gui')")
    vim.cmd('hi! CurrFile guibg=' .. bg_color .. ' guifg=' .. fg_color_icon)
    return '%#CurrFile#' .. icon .. '%#StatusLine#' .. blank
end


local function git_changes(fp)
  local j = Job:new({
    command = "git",
    args = {"status", "--short"},
    cwd = vim.fn.fnamemodify(fp, ":h"),
  })

  local ok, _ = pcall(function()
    return vim.trim(j:sync()[1])
  end)

  if ok then
    return "~"
  else
    return ""
  end
end


local function get_git_remote(fp)
  local j = Job:new({
    command = "git",
    args = {"config", "--get", "remote.origin.url"},
    cwd = vim.fn.fnamemodify(fp, ":h"),
  })

  local ok, result = pcall(function()
    return vim.trim(j:sync()[1])
  end)

  if ok then
    return result
  end
end


local function get_git_branch(fp)
  local j = Job:new({
    command = "git",
    args = {"branch", "--show-current"},
    cwd = vim.fn.fnamemodify(fp, ":h"),
  })

  local ok, result = pcall(function()
    return vim.trim(j:sync()[1])
  end)

  if ok then
    return result
  else
    return false
  end
end

local function git_part(fp)
    local branch = get_git_branch(fp)
    if branch == false then
        return false
    else
        local remote = get_git_remote(fp)
        if remote == nil then
            local dot = git_changes(fp)
            local string = "[ "..branch..dot.."]"
            return string
        else
            local dot = git_changes(fp)
            local string = "[ "..branch..dot.."]"
            return string
        end
    end
end

local function check_for_new_string(fp)
    if curr_files[fp] ~= nil then
        if curr_files[fp] == false then
            return ""
        else
            return curr_files[fp]
        end
    else
        local new_string = git_part(fp)
        curr_files[fp] = new_string
        if new_string == false then
            return ""
        else
            return new_string
        end
    end
end

local function statuslinee(active)
    local icon = file_icon()
    if not active then
        local line = "%#StatusLineNC#"..blank.."%f"
        return line
    else
        local mode = vim.api.nvim_get_mode()['mode']
        RedrawColors(mode)
        local mode_string = "%#Mode#"..current_mode[mode].."%#StatusLine#"
        local fp = vim.api.nvim_buf_get_name(string.format(vim.api.nvim_get_current_buf()))
        local git_string = string.format(check_for_new_string(fp))
        local line_numbers = "%#StatusLineNumbers# [%l/%L|%p%%] %#StatusLine#"
        local left = blank..icon.."%f"..blank..git_string..line_numbers
        local right = "%="
        local lsp = lsp_status.status()
        return mode_string..left..right..lsp
    end
end

local function exec_autocommands()
    _G.statuslinee = statuslinee
    vim.cmd([[autocmd WinEnter,BufEnter * setlocal statusline=%!v:lua.statuslinee(1)]])
    vim.cmd([[autocmd WinLeave,BufLeave * setlocal statusline=%!v:lua.statuslinee()]])
end
exec_autocommands()

