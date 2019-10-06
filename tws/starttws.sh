/usr/bin/Xvfb :0 -ac -screen 0 1024x768x24 &
sleep 5 && /usr/bin/x11vnc -nonc -viewpasswd remote_view_only_pass -passwd some_pass123 -display :0 -forever -shared -noipv6 -localhost &
sleep 10 && ~/Jts/978/tws
