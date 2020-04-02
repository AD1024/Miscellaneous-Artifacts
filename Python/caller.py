import subprocess
import os
import signal

if __name__ == '__main__':
    try:
        process = subprocess.run(['python3', 'callee.py'], timeout=5, stdout=subprocess.PIPE)
    except:
        pass
        print(process.stdout)
    # try:
    #     process.wait(3)
    # except TimeoutError as e:
    #     print('Timeout')
    #     print(process.pid)
    #     os.kill(process.pid, signal.SIGTERM)