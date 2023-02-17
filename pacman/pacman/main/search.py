# search.py
# ---------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
#
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


"""
In search.py, you will implement generic search algorithms which are called by
Pacman agents (in searchAgents.py).
"""

from builtins import object
import util


class SearchProblem(object):
    """
    This class outlines the structure of a search problem, but doesn't implement
    any of the methods (in object-oriented terminology: an abstract class).

    You do not need to change anything in this class, ever.
    """

    def getStartState(self):
        """
        Returns the start state for the search problem.
        """
        #return self.
        #print("startstate=", self.getStartState())
        #util.raiseNotDefined()

    def isGoalState(self, state):
        """
          state: Search state

        Returns True if and only if the state is a valid goal state.
        """
        #return self.state == state
        #print("state=", self.getStartState())
        #util.raiseNotDefined()

    def getSuccessors(self, state):
        """
          state: Search state

        For a given state, this should return a list of triples, (successor,
        action, stepCost), where 'successor' is a successor to the current
        state, 'action' is the action required to get there, and 'stepCost' is
        the incremental cost of expanding to that successor.
        """
        util.raiseNotDefined()

    def getCostOfActions(self, actions):
        """
         actions: A list of actions to take

        This method returns the total cost of a particular sequence of actions.
        The sequence must be composed of legal moves.
        """
        util.raiseNotDefined()


def tinyMazeSearch(problem):
    """
    Returns a sequence of moves that solves tinyMaze.  For any other maze, the
    sequence of moves will be incorrect, so only use this for tinyMaze.
    """
    from game import Directions

    s = Directions.SOUTH
    w = Directions.WEST
    return [s, s, w, s, w, w, s, w]


def depthFirstSearch(problem):
    """
    Search the deepest nodes in the search tree first.

    Your search algorithm needs to return a list of actions that reaches the
    goal. Make sure to implement a graph search algorithm.

    To get started, you might want to try some of these simple commands to
    understand the search problem that is being passed in:

    print "Start:", problem.getStartState()
    print "Is the start a goal?", problem.isGoalState(problem.getStartState())
    print "Start's successors:", problem.getSuccessors(problem.getStartState())
    """
   # print("getStartState()= ", problem.getStartState())
    root = {'state': problem.getStartState(),
            'action': [],
            'cost': 0}
    stack = util.Stack() # Stack based approach
    stack.push(root)
    explored = [] # To keep track of visited nodes
    result = [] # List to return the result in form of action sequence

    while not stack.isEmpty():
        vertex = stack.pop()

        if problem.isGoalState(vertex['state']):
            #print("goalstate= ", vertex['state'])
            result.append(vertex['action'])
            return result[0]
        
        if vertex['state'] not in explored:
            explored.append(vertex['state']) # update visited list
           # print("getSuccessors()= ", problem.getSuccessors(vertex['state']))
            for neighbor in problem.getSuccessors(vertex['state']): # For every successor of unvisited vertex, push the action sequence and cost sequence along with state
                #print("cost of action= ",neighbor[2]," for neighbour=", neighbor[0]," to state= ", vertex['state'])
                stack.push({'state': neighbor[0],
                            'action': vertex['action'] + [neighbor[1]],
                            'cost': vertex['cost'] + neighbor[2]})


def breadthFirstSearch(problem):
    """Search the shallowest nodes in the search tree first."""
    "*** YOUR CODE HERE ***"
    #util.raiseNotDefined()
    root = {'state': problem.getStartState(),
            'action': [],
            'cost': 0}
    queue = util.Queue()  # Queue based approach
    queue.push(root)
    explored = []  # To keep track of visited nodes
    result = []  # List to return the result in form of action sequence

    while not queue.isEmpty():
        vertex = queue.pop()

        if problem.isGoalState(vertex['state']):
            result.append(vertex['action'])
            return result[0]

        if vertex['state'] not in explored:
            explored.append(vertex['state'])  # update visited list
            for neighbor in problem.getSuccessors(vertex[
                                                      'state']):  # For every successor of unvisited vertex, push the action sequence and cost sequence along with state
                queue.push({'state': neighbor[0],
                            'action': vertex['action'] + [neighbor[1]],
                            'cost': vertex['cost'] + neighbor[2]})


def uniformCostSearch(problem, heuristic=None):
    """Search the node of least total cost first."""
    "*** YOUR CODE HERE ***"
   # util.raiseNotDefined()
    root = {'state': problem.getStartState(),
            'action': [],
            'cost': 0}
    PriorityQueue = util.PriorityQueue()  # Priority Queue based approach
    PriorityQueue.push(root, root['cost'])
    explored = []  # To  track of visited nodes
    result = []  # List to return the result in form of action sequence
  #  PriorityQueue=util.PriorityQueueWithFunction(PriorityQueue_obj)

    while not PriorityQueue.isEmpty():
        vertex = PriorityQueue.pop()
        #print('current vertex=', vertex['state'], ' cost= ', vertex['cost'])

        #if problem.isGoalState(vertex['state']):
        #    result.append(vertex['action'])
        #    return result[0]

        if vertex['state'] not in explored:
            explored.append(vertex['state'])  # update visited list

            if problem.isGoalState(vertex['state']):
                result.append(vertex['action'])
                return result[0]

            for neighbor in problem.getSuccessors(vertex[
                                                      'state']):  # For every successor of unvisited vertex, push the action sequence and cost sequence along with state
                #print("neighbour=", vertex['state'], "cost=", vertex['cost'])
                if neighbor[0] not in explored:
                    PriorityQueue.push({'state': neighbor[0],
                                'action': vertex['action'] + [neighbor[1]],
                                'cost': vertex['cost'] + neighbor[2]}, vertex['cost'] + neighbor[2])



def nullHeuristic(state, problem=None):
    """
    A heuristic function estimates the cost from the current state to the nearest
    goal in the provided SearchProblem.  This heuristic is trivial.
    """
    return 0


def aStarSearch(problem, heuristic=nullHeuristic):
    """Search the node that has the lowest combined cost and heuristic first."""
    "*** YOUR CODE HERE ***"
    #util.raiseNotDefined()
    root = {'state': problem.getStartState(),
            'action': [],
            'cost': 0}
    PriorityQueue = util.PriorityQueue()  # Priority queue based approach
    PriorityQueue.push(root, root['cost'])
    explored = []  # To  track of visited nodes
    result = []  # List to return the result in form of action sequence
    #  PriorityQueue=util.PriorityQueueWithFunction(PriorityQueue_obj)

    while not PriorityQueue.isEmpty():
        vertex = PriorityQueue.pop()
        # print('current vertex=', vertex['state'], ' cost= ', vertex['cost'])

        # if problem.isGoalState(vertex['state']):
        #    result.append(vertex['action'])
        #    return result[0]

        if vertex['state'] not in explored:
            explored.append(vertex['state'])  # update visited list

            if problem.isGoalState(vertex['state']):
                result.append(vertex['action'])
                return result[0]

            for neighbor in problem.getSuccessors(vertex[
                                                      'state']):  # For every successor of unvisited vertex, push the action sequence and cost sequence along with state
                # print("neighbour=", vertex['state'], "cost=", vertex['cost'])
                if neighbor[0] not in explored:
                    heuristic_fn_plus_gn = vertex['cost']+neighbor[2]+heuristic(neighbor[0], problem) #calculate h(n)+g(n)
                    PriorityQueue.push({'state': neighbor[0],
                                        'action': vertex['action'] + [neighbor[1]],
                                        'cost': vertex['cost'] + neighbor[2]}, heuristic_fn_plus_gn)
                    # 'cost': neighbor[2]}, 'cost')



# Abbreviations
bfs = breadthFirstSearch
dfs = depthFirstSearch
astar = aStarSearch
ucs = uniformCostSearch
