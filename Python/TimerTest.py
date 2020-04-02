import threading


def test(run_span, run_time):
    print(run_span, run_time, run_span * run_time)
    threading.Timer(run_span, test, [run_span, run_time + 1]).start()

if __name__ == '__main__':
    test(5, 0)