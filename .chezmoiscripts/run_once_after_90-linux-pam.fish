#!/bin/fish

if ! grep /etc/pam.d/login -qe kwallet
    echo "auth optional pam_kwallet5.so" |
        sudo tee -a /etc/pam.d/login
    echo "session optional pam_kwallet5.so auto_start force_run" |
        sudo tee -a /etc/pam.d/login
end
