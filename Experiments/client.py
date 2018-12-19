import socket

host_ip, server_port = "", 9876

tcp_client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

data = "((:= x 1)(:= 2 4))"

try:
    tcp_client.connect((host_ip,server_port))
    tcp_client.sendall(data.encode())

    received = tcp_client.recv(2048)
finally:
    tcp_client.close()

print( " Bytes sent:  {}".format(data))
print("Bytes received: {}".format(received.decode()))

