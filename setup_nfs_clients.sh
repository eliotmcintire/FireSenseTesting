#!/bin/bash
set -euo pipefail

SERVER_NAME="${1:?Usage: $0 <server_hostname>}"
SERVER_IP="132.156.148.169"
SHARE="/srv/shared_cache"
MOUNT="/mnt/shared_cache"
MACHINES="abies acer birds camas carbon caribou coco core dougfir fire mega mpb pinus sbw"
SSH_CONFIG="$HOME/.ssh/config"

NFS_FSTAB="${SERVER_IP}:${SHARE}  ${MOUNT}  nfs  defaults,_netdev  0  0"
BIND_FSTAB="${SHARE}  ${MOUNT}  none  bind  0  0"

for host in $MACHINES; do
    echo "============================================"
    echo "Setting up: $host"
    echo "============================================"

    # --- mega (the server itself): use bind mount ---
    if [ "$host" = "$SERVER_NAME" ]; then
        echo "  -> This is the NFS server: using bind mount locally"

        sudo mkdir -p "$MOUNT"
        if ! mountpoint -q "$MOUNT"; then
            sudo mount --bind "$SHARE" "$MOUNT"
            echo "  -> Bind mounted ${SHARE} -> ${MOUNT}"
        else
            echo "  -> Already mounted, skipping"
        fi
        if ! grep -qF "$BIND_FSTAB" /etc/fstab; then
            echo "$BIND_FSTAB" | sudo tee -a /etc/fstab > /dev/null
            echo "  -> Added bind mount to /etc/fstab"
        else
            echo "  -> fstab entry already exists, skipping"
        fi

    # --- All other machines: push SSH config + NFS client ---
    else
        echo "  -> Pushing SSH config"
        scp -q "$SSH_CONFIG" "${host}:.ssh/config"

        echo "  -> NFS client setup"
        ssh "$host" bash -s <<REMOTE
            if ! dpkg -s nfs-common &>/dev/null; then
                sudo apt-get update -qq
                sudo apt-get install -y -qq nfs-common
                echo '  -> Installed nfs-common'
            else
                echo '  -> nfs-common already installed'
            fi

            sudo mkdir -p ${MOUNT}

            if ! mountpoint -q ${MOUNT}; then
                sudo mount -t nfs ${SERVER_IP}:${SHARE} ${MOUNT}
                echo '  -> Mounted ${SERVER_IP}:${SHARE} -> ${MOUNT}'
            else
                echo '  -> Already mounted, skipping'
            fi

            if ! grep -qF '${NFS_FSTAB}' /etc/fstab; then
                echo '${NFS_FSTAB}' | sudo tee -a /etc/fstab > /dev/null
                echo '  -> Added NFS mount to /etc/fstab'
            else
                echo '  -> fstab entry already exists, skipping'
            fi
REMOTE
    fi

    echo ""
done

echo "Done. All machines configured."
