function load_s3
    set SOPS_FILE "$HOME/Documents/Dots/server_infra.yaml"

    if not type -q jq
        echo "Fehler: 'jq' fehlt."
        return 1
    end

    if not test -f "$SOPS_FILE"
        echo "Fehler: Datei nicht gefunden."
        return 1
    end

    sops -d --output-type json "$SOPS_FILE" | jq -r '.s3_hetzner | 
        "set -gx AWS_DEFAULT_REGION \"\(.s3_region)\";",
        "set -gx RESTIC_REPOSITORY \"s3:\(.s3_endpoint)/\(.s3_bucket)\";",
        "set -gx AWS_ACCESS_KEY_ID \"\(.access_key)\";",
        "set -gx AWS_SECRET_ACCESS_KEY \"\(.secret_key)\";",
        "set -gx RESTIC_PASSWORD \"\(.restic_pw)\";"
    ' | source

    echo "✅ S3 Environment geladen!"
end
