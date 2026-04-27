#!/usr/bin/env fish

uv tool install kptncook
uv tool install --with-executables-from ansible-core,ansible-lint --with requests ansible

if test (uname) = Darwin
    uv tool install ocrmypdf --with ocrmypdf-appleocr
else
    uv tool install ocrmypdf
end

uv tool install "zotero-mcp-server[all]"
uv tool install basic-memory
uv tool install things-mcp
