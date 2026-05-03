#!/bin/bash

set -e

# --- Configuration ---
SOURCE_DIR="$1"
DEST_DIR="$2"
GPG_ALGO="AES256"

# --- Validation ---
if [[ -z "$SOURCE_DIR" || -z "$DEST_DIR" ]]; then
    echo "Usage: $0 <source_directory> <destination_directory>"
    exit 1
fi

if [[ ! -d "$SOURCE_DIR" ]]; then
    echo "Error: Source directory '$SOURCE_DIR' not found."
    exit 1
fi

# --- Passphrase Handling ---
echo -n "Enter Encryption Passphrase: "
read -s PASSPHRASE
echo
echo -n "Confirm Passphrase: "
read -s PASSPHRASE_CONFIRM
echo

if [[ "$PASSPHRASE" != "$PASSPHRASE_CONFIRM" ]]; then
    echo "Error: Passphrases do not match."
    exit 1
fi

# --- Encryption Process ---
echo "Starting encryption from $SOURCE_DIR to $DEST_DIR..."

# Create base destination directory
mkdir -p "$DEST_DIR"

# Find all files, create directory structure, and encrypt
find "$SOURCE_DIR" -type f | while read -r FILE; do
    
    # Get relative path
    REL_PATH="${FILE#$SOURCE_DIR/}"
    
    # Create target directory path
    TARGET_FILE="$DEST_DIR/$REL_PATH.gpg"
    mkdir -p "$(dirname "$TARGET_FILE")"
    
    echo "Encrypting: $REL_PATH"
    
    # Encrypt via pipe
    echo "$PASSPHRASE" | gpg --batch --yes --passphrase-fd 0 \
        --cipher-algo $GPG_ALGO -c -o "$TARGET_FILE" "$FILE"
done

echo "Encryption complete."
