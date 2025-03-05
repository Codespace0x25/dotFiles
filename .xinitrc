# Disable access control for the current user.
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Set default cursor.
xsetroot -cursor_name left_ptr

exec /home/olivia/Documents/client/echo/src/main &

# Set keyboard repeat rate.
xset r rate 200 60

export QT_QPA_PLATFORMTHEME="qt5ct"                # Set Qt platform theme
export GTK_THEME="catppuccin-macchiato-maroon-standard+default"  # Set GTK theme
export GTK_ICON_THEME="Adwaita"                    # Set GTK icon theme
export GDK_SCALE=1                                 # Set scale for GTK
export GDK_DPI_SCALE=1                             # Set DPI scale for GTK
export NIXPKGS_ALLOW_UNFREE=1
export EDITOR="emacsclient -c -a ''"
export VISUAL="emacsclient -c -a ''"

# Finally start Emacs
exec emacsclient -a "" -c
 
