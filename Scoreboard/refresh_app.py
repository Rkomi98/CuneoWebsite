from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from multiprocessing import Process
import time
from VolleyballScoreboardServer import app

watch_directory = "C:/Users/mirko/Documents/GitHub/CuneoWebsite.io/Scoreboard"
file_extension = ".dvw"

class ChangeHandler(FileSystemEventHandler):
    def __init__(self, restart_function):
        super().__init__()
        self.restart_function = restart_function

    def on_modified(self, event):
        if event.src_path.endswith(file_extension):
            print(f"Detected change in: {event.src_path}")
            self.restart_function()

def run_server():
    app.run(debug=False)

def restart_server():
    global server_process
    if server_process:
        server_process.terminate()
        server_process.join()
    server_process = Process(target=run_server)
    server_process.start()

def start_observer():
    global server_process
    server_process = None
    restart_server()

    event_handler = ChangeHandler(restart_server)
    observer = Observer()
    observer.schedule(event_handler, path=watch_directory, recursive=False)
    observer.start()

    try:
        while True:
            time.sleep(2)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()
