What is Software Transactional Memory (STM)?

Software Transactional Memory (STM) is a concurrency programming solution that enables multiple threads to access shared data in a concurrent and safe manner, without requiring traditional locking and synchronization methods. Instead, STM uses a transaction-like mechanism to handle concurrent access to shared data. In a nutshell, STM lets threads execute code concurrently without interference, and only upon completion of all threads, the results are merged.

How does STM work?

STM introduces the concept of a "transaction" as a unit of work performed by a thread on a shared data structure. A transaction consists of a sequence of atomic operations on shared data.

When a thread starts a transaction, it first creates a "log" of its intended operations. The log is a copy of the shared data structure that the thread will modify. The thread then performs its operations on the log, rather than the shared data itself.

Once the thread completes its transaction, it attempts to "commit" the transaction by applying its changes to the shared data. However, before committing, the system checks for potential conflicts with other threads' transactions. If a conflict is detected, the transaction is "aborted" and the thread's operations are discarded. The thread can then retry the transaction.

Example: Traditional multithreading with locks

Here is a simplified example of how multithreading might be implemented using traditional locking mechanisms:


Copy
Copied!
import threading

class LockBasedData:
    def __init__(self, value=0):
        self.value = value
        self.lock = threading.Lock()

    def increment(self):
        self.lock.acquire()
        self.value += 1
        self.lock.release()

def increment_using_lock(data):
    data.increment()

def increment_using_lock_thread(data):
    t = threading.Thread(target=increment_using_lock, args=(data,))
    t.start()

data = LockBasedData()
increment_using_lock_thread(data)
increment_using_lock_thread(data)
In this example, the LockBasedData class has a value attribute that is shared across multiple threads. The increment method is responsible for incrementing the value, using a threading.Lock to ensure that only one thread can modify the value at a time.

Example: Software Transactional Memory (STM)


Copy
Copied!
from stm import STMManager

class STMData:
    def __init__(self, value=0):
        self.value = value
        self.manager = STMManager()

    def increment(self):
        with self.manager.start_transaction() as log:
            log["value"] += 1
        self.manager.commit_transaction(log)

def increment_using_stm(data):
    data.increment()

def increment_using_stm_thread(data):
    t = threading.Thread(target=increment_using_stm, args=(data,))
    t.start()

data = STMData()
increment_using_stm_thread(data)
increment_using_stm_thread(data)
In this example, we have an STMData class that uses STM to manage concurrent access to its value attribute.

Before making any changes to the shared data, the increment method starts a transaction using the STMManager instance. The transaction creates a log, and the thread updates the log with its intended changes.

Upon completing the transaction, the thread attempts to commit the changes by applying the log to the shared data. If there are no conflicts, the changes are applied, and the transaction is successful. If a conflict is detected, the transaction is aborted, and the thread retries the operation.

Note that STM implementations can vary in details, but this example provides a general illustration of the concept.

Comparison

While both traditional locking and STM can achieve correctness and safety in concurrent programming, STM offers several advantages:

Less Overhead: STM eliminates the overhead of acquiring and releasing locks, which can be significant in high-contention scenarios.
No Deadlocks: STM reduces the risk of deadlocks, as transactions are atomic and can be rolled back in case of conflicts.
More Parallelism: STM allows for more parallelism as threads can execute in isolation, without blocking each other.
However, STM also has some limitations:

Overhead of Bookkeeping: STM introduces extra overhead in tracking and logging transactions, which can be significant for some data structures.
Potential for Conflicts: STM can still experience conflicts, which can lead to transaction aborts and re-tries.
In practice, STM implementations often use hybrid approaches, combining traditional locking with STM-like transactions, to balance performance and concurrency.