#!/usr/bin/env fish

uv tool install pytr
uv tool install kptncook
uv tool install --with-executables-from ansible-core,ansible-lint --with requests ansible
