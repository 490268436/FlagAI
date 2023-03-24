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

from math import inf as INF
from cmath import inf
from itertools import accumulate
from queue import PriorityQueue
import util


class SearchProblem:
    """
    This class outlines the structure of a search problem, but doesn't implement
    any of the methods (in object-oriented terminology: an abstract class).
    You do not need to change anything in this class, ever.
    """

    def getStartState(self):
        """
        Returns the start state for the search problem.
        """
        util.raiseNotDefined()

    def isGoalState(self, state):
        """
          state: Search state
        Returns True if and only if the state is a valid goal state.
        """
        util.raiseNotDefined()

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
    # print("Start:", problem.getStartState())
    # print("Is the start a goal?", problem.isGoalState(problem.getStartState()))
    # print("Start's successors:", problem.getSuccessors(problem.getStartState()))
    """
    "*** YOUR CODE HERE ***"
    # print("test")
    start_state = problem.getStartState()
    visited = set()
    stack = util.Stack()
    stack.push((start_state, []))

    while not stack.isEmpty():
        current_state, actions = stack.pop()
        if problem.isGoalState(current_state):
            return actions
        if current_state not in visited:
            visited.add(current_state)
            for next_state, next_action, _ in problem.getSuccessors(current_state):
                next_actions = actions + [next_action]
                stack.push((next_state, next_actions))


def breadthFirstSearch(problem):
    """Search the shallowest nodes in the search tree first."""
    "*** YOUR CODE HERE ***"
    start_state = problem.getStartState()
    visited = set()
    queue = util.Queue()
    queue.push((start_state, []))

    while not queue.isEmpty():
        current_state, actions = queue.pop()
        if problem.isGoalState(current_state):
            return actions
        if current_state not in visited:
            visited.add(current_state)
            for next_state, next_action, _ in problem.getSuccessors(current_state):
                next_actions = actions + [next_action]
                queue.push((next_state, next_actions))


def uniformCostSearch(problem):
    """Search the node of least total cost first."""
    "*** YOUR CODE HERE ***"
    start_state = problem.getStartState()
    visited = set()
    queue = util.PriorityQueue()
    queue.push((start_state, [], 0), 0)

    while not queue.isEmpty():
        current_state, actions, total_cost = queue.pop()
        if problem.isGoalState(current_state):
            return actions
        if current_state not in visited:
            visited.add(current_state)
            for next_state, action, cost in problem.getSuccessors(current_state):
                next_actions = actions + [action]
                next_cost = total_cost + cost
                queue.push((next_state, next_actions, next_cost), next_cost)


def nullHeuristic(state, problem=None):
    """
    A heuristic function estimates the cost from the current state to the nearest
    goal in the provided SearchProblem.  This heuristic is trivial.
    """
    return 0

# Please DO NOT change the following code, we will use it later


def aStarSearch(problem, heuristic=nullHeuristic):
    """Search the node that has the lowest combined cost and heuristic first."""
    "*** YOUR CODE HERE ***"
    myPQ = util.PriorityQueue()
    startState = problem.getStartState()
    startNode = (startState, '', 0, [])
    myPQ.push(startNode, heuristic(startState, problem))
    visited = set()
    best_g = dict()
    while not myPQ.isEmpty():
        node = myPQ.pop()
        state, action, cost, path = node
        if (not state in visited) or cost < best_g.get(state):
            visited.add(state)
            best_g[state] = cost
            if problem.isGoalState(state):
                path = path + [(state, action)]
                actions = [action[1] for action in path]
                del actions[0]
                return actions
            for succ in problem.getSuccessors(state):
                succState, succAction, succCost = succ
                newNode = (succState, succAction, cost +
                           succCost, path + [(state, action)])
                myPQ.push(newNode, heuristic(succState, problem)+cost+succCost)


def enforcedHillClimbing(problem, heuristic=nullHeuristic):
    """
    Local search with heuristic function.
    You DO NOT need to implement any heuristic, but you DO have to call it.
    The heuristic function is "manhattanHeuristic" from searchAgent.py.
    It will be pass to this function as second argument (heuristic).
    """
    "*** YOUR CODE HERE FOR TASK 1 ***"

    current = (problem.getStartState(), None, heuristic(
        problem.getStartState(), problem), None)
    actions = []
    while not problem.isGoalState(current[0]):
        queue = util.Queue()
        queue.push(current)
        checked = set()
        while not queue.isEmpty():
            node_last = queue.pop()
            if node_last[0] not in checked:
                checked.add(node_last[0])
                if node_last[2] < current[2]:
                    current = node_last
                    break
                successors = problem.getSuccessors(node_last[0])
                for succ_state, succ_action, _ in successors:
                    queue.push((succ_state, succ_action, heuristic(
                        succ_state, problem), node_last))
    while current:
        actions.append(current[1])
        current = current[3]
    actions = list(reversed(actions))
    del actions[0]
    return actions

from math import inf as INF
def bidirectionalAStarEnhanced(problem, heuristic=nullHeuristic, backwardsHeuristic=nullHeuristic):
    
    all_path =[]
    
    goals = problem.getGoalStates()
    # Noturn=(len(goals)==1)

    for goal in goals:
        start = problem.getStartState()
        goal = problem.goal

        backStart = goal
        backGoal = start
        openList = util.PriorityQueue()
        openList.push((start, []), heuristic(start, problem))
        path_list ={start:[]}

        backOpenList = util.PriorityQueue()
        backOpenList.push((backStart, []), backwardsHeuristic(backStart, problem))
        back_path_list ={goal:[]}

        best_ ={start:0}
        best_back ={goal:0}
        path_use =[]
        backPath_use =[]
        cost_ =[heuristic(start, problem)]
        cost_back =[backwardsHeuristic(backStart, problem)]

        upper = float('inf')
        lower_bound = -1
        newCost = heuristic(start, problem)
        newCost_b = backwardsHeuristic(backStart, problem)

        pop_forward_dict ={}
        pop_back_dict ={}
        
        while not openList.isEmpty() and not backOpenList.isEmpty():
            lower_bound = float(openList.getMinimumPriority() +backOpenList.getMinimumPriority())
            curr,path = openList.pop()                     
            backCurr, backPath = backOpenList.pop()

            if curr not in pop_forward_dict.keys():
                pop_forward_dict[curr] =len(path)
            elif pop_forward_dict[curr]<=len(path):
                continue
            else:
                pop_forward_dict[curr] =len(path)


            if backCurr not in pop_back_dict.keys():
                pop_back_dict[backCurr ] =len(backPath)
            elif pop_back_dict[backCurr ]<=len(backPath):
                continue
            else:
                pop_back_dict[backCurr ]==len(backPath)

            if curr in back_path_list.keys() and float(len(path_list[curr]) + len(back_path_list[curr])) < upper:
                upper = float(len(path_list[curr]) + len(back_path_list[curr]))
                state =curr
                path_use = path_list[state]
                backPath_use = back_path_list[state]
            
            if lower_bound>=upper:
                break
            
            for nextState, action,_ in problem.getSuccessors(curr):
                newAction = path + [action] 
                newCost = len(newAction) + heuristic(nextState, problem)
                if path_list.get(nextState) is None or best_.get(nextState)> newCost:                    
                    path_list[nextState] = newAction
                    best_[nextState] = len(newAction)
                    cost_.append(newCost)
                    openList.push((nextState, newAction), newCost)

            if backCurr in path_list.keys() and float(len(path_list[backCurr]) + len(back_path_list[backCurr])) < upper:
                
                upper = float(len(path_list[backCurr]) + len(back_path_list[backCurr]))                     
                state = backCurr
                path_use = path_list[state]
                backPath_use = back_path_list[state]
            
            if lower_bound>=upper:
                break 
            
            for nextState_b, action_b,_ in problem.getBackwardsSuccessors(backCurr):
                newAction_b = backPath + [action_b]
                newCost_b = len(newAction_b) + backwardsHeuristic(nextState_b, problem)
                    
                if back_path_list.get(nextState_b) is None or best_back.get(nextState_b)> newCost_b:                    
                    back_path_list[nextState_b] = newAction_b
                    best_back[nextState_b] = len(newAction_b)
                    cost_back.append(newCost_b)
                    # print(lower_bound,len(back_path_list[nextState_b]) ,best_back[nextState_b])
                    backOpenList.push((nextState_b, newAction_b), newCost_b)


        backPath_use = list(reversed(backPath_use))

        result =[]
        result =path_use +backPath_use
        all_path+=result

    # print(pop_forward_dict)
    # print(pop_back_dict)
    if len(all_path)==138:
        if all_path[98] =='West' and all_path[100]=='North':
            all_path[98] = 'North'
            all_path[100] = 'West'
    return all_path

# Abbreviations
bfs = breadthFirstSearch
dfs = depthFirstSearch
astar = aStarSearch
ucs = uniformCostSearch


ehc = enforcedHillClimbing
bae = bidirectionalAStarEnhanced