import StringIO
import pstats
import sys

from Pymacs import lisp

class EmacsStats(object):

    def __init__(self, stat_file):
        self._stdout = StringIO.StringIO()
        self._stats = None
        self.load(stat_file)

    def load(self, stat_file):
        old_stdout = sys.stdout
        old_stderr = sys.stderr
        sys.stdout = self._stdout
        sys.stderr = self._stdout
        try:
            self._stats = pstats.Stats( stat_file )
            self._stats.strip_dirs( )
            self._stats.sort_stats( 'time' )
        finally:
            sys.stdout = old_stdout
            sys.stderr = old_stderr    

    def sort(self, order):
        self._stats.sort_stats(order)
        
    def display(self, number_items):
        self._stdout.truncate(0)
        self._stats.print_stats(number_items)
        return self._stdout.getvalue()

    def displayCallees(self, number_items, f):
        self._stdout.truncate(0)
        self._stats.print_callees(f, number_items)
        return self._stdout.getvalue()

    def displayCallers(self, number_items, f):
        self._stdout.truncate(0)
        self._stats.print_callers(f, number_items)
        return self._stdout.getvalue()
    
