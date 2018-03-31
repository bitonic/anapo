import http.server
import socketserver

class Handler(http.server.SimpleHTTPRequestHandler):
  def do_GET(self):
    if self.path.endswith(".js"):
      self.path = "/dist/build/benchmark/benchmark.jsexe" + self.path
    else:
      self.path = "/index.html"
    super().do_GET()

server_address = ('', 8000)
httpd = http.server.HTTPServer(server_address, Handler)
httpd.serve_forever()
