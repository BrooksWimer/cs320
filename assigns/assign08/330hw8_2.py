
def GetOpt(operations, OPT, i, A): 
    max_sum = float('-inf')  # Use float('-inf') for safety
    best_solution = [None, None, None]  # Assuming False/True represents the operation
    for ops, sum_val in operations:
        if sum_val > max_sum:
            max_sum = sum_val
            best_solution = ops

    curr = OPT[i-3]
    new_sums = []
    for x in [0, 1, 2]: 
        # Apply operation based on best_solution. Adjust curr accordingly.
        if best_solution[x]:
            curr += A[i-(2-x)]
        else: 
            curr -= A[i-(2-x)]
        new_sums.append(curr)

    return best_solution, new_sums



def BuildOpt(A, operations, OPT): 
    for i in range(2, len(A)): 
        if (i >= 5) and  (operations[i-3] == False) and (operations[i-4] == False) and (operations[i-5] == False):
            new_operations, sum = GetOpt(	
            [[[True, True, True], OPT[i-3] + A[i-2] + A[i-1] + A[i]],
            [[True, True, False], OPT[i-3] + A[i-2] + A[i-1] - A[i]],
            [[True, False, True], OPT[i-3] + A[i-2] - A[i-1] + A[i]],
            [[True, False, False], OPT[i-3] + A[i-2] - A[i-1] - A[i]]],
            OPT, i, A
            )
            operations[i-2:i+1] = new_operations
            OPT[i-2:i+1] = sum 

        elif (i >= 5) and (operations[i-3] == True) and (operations[i-4] == True) and (operations[i-5] == True):
            new_operations, sum= GetOpt(	
            [[[False, True, True], OPT[i-3] - A[i-2] + A[i-1] + A[i]],
            [[False, False, True], OPT[i-3] - A[i-2] - A[i-1] + A[i]],
            [[False, True, False], OPT[i-3] - A[i-2] + A[i-1] - A[i]],
            [[False, False, False], OPT[i-3] - A[i-2] - A[i-1] - A[i]]],
            OPT, i, A
            )
            operations[i-2:i+1] = new_operations
            OPT[i-2:i+1] = sum 
            
        elif i >= 4: 
            if (operations[i-3] == False) and (operations[i-4] == False):
                new_operations, sum= GetOpt(	
                [[[True, True, True], OPT[i-3] + A[i-2] + A[i-1] + A[i]],
                [[True, True, False], OPT[i-3] + A[i-2] + A[i-1] - A[i]],
                [[True, False, True], OPT[i-3] + A[i-2] - A[i-1] + A[i]],
                [[True, False, False], OPT[i-3] + A[i-2] - A[i-1] - A[i]],
                [[False, True, True], OPT[i-3] - A[i-2] + A[i-1] + A[i]],
                [[False, True, False], OPT[i-3] - A[i-2] + A[i-1] - A[i]]],
                OPT, i, A
                )
                operations[i-2:i+1] = new_operations
                OPT[i-2:i+1] = sum 

            elif (operations[i-3] == False):
                new_operations, sum = GetOpt(	
                [[[True, True, True], OPT[i-3] + A[i-2] + A[i-1] + A[i]],
                [[True, True, False], OPT[i-3] + A[i-2] + A[i-1] - A[i]],
                [[True, False, True], OPT[i-3] + A[i-2] - A[i-1] + A[i]],
                [[True, False, False], OPT[i-3] + A[i-2] - A[i-1] - A[i]],
                [[False, True, True], OPT[i-3] - A[i-2] + A[i-1] + A[i]],
                [[False, False, True], OPT[i-3] - A[i-2] - A[i-1] + A[i]],
                [[False, True, False], OPT[i-3] - A[i-2] + A[i-1] - A[i]]],
                OPT, i, A
                )
                operations[i-2:i+1] = new_operations
                OPT[i-2:i+1] = sum 

            elif (operations[i-3] == True) and (operations[i-4] == True):
                new_operations, sum = GetOpt(	
                [[[False, True, True], OPT[i-3] - A[i-2] + A[i-1] + A[i]],
                [[False, False, True], OPT[i-3] - A[i-2] - A[i-1] + A[i]],
                [[False, True, False], OPT[i-3] - A[i-2] + A[i-1] - A[i]],
                [[False, False, False], OPT[i-3] - A[i-2] - A[i-1] - A[i]],
                [[True, False, True], OPT[i-3] + A[i-2] - A[i-1] + A[i]],
                [[True, False, False], OPT[i-3] + A[i-2] - A[i-1] - A[i]]],
                OPT, i, A
                )
                operations[i-2:i+1] = new_operations
                OPT[i-2:i+1] = sum 

            elif (operations[i-3] == True):
                new_operations, sum = GetOpt(	
                [[[False, True, True], OPT[i-3] - A[i-2] + A[i-1] + A[i]],
                [[False, False, True], OPT[i-3] - A[i-2] - A[i-1] + A[i]],
                [[False, True, False], OPT[i-3] - A[i-2] + A[i-1] - A[i]],
                [[False, False, False], OPT[i-3] - A[i-2] - A[i-1] - A[i]],
                [[True, True, False], OPT[i-3] + A[i-2] + A[i-1] - A[i]],
                [[True, False, True], OPT[i-3] + A[i-2] - A[i-1] + A[i]],
                [[True, False, False], OPT[i-3] + A[i-2] - A[i-1] - A[i]]],
                OPT, i, A
                )
                operations[i-2:i+1] = new_operations 
                OPT[i-2:i+1] = sum 

        else:
            new_operations, sum = GetOpt(	
            [[[True, True, True], OPT[i-3] + A[i-2] + A[i-1] + A[i]],
            [[True, True, False], OPT[i-3] + A[i-2] + A[i-1] - A[i]],
            [[True, False, True], OPT[i-3] + A[i-2] - A[i-1] + A[i]],
            [[True, False, False], OPT[i-3] + A[i-2] - A[i-1] - A[i]],
            [[False, True, True], OPT[i-3] - A[i-2] + A[i-1] + A[i]],
            [[False, False, True], OPT[i-3] - A[i-2] - A[i-1] + A[i]],
            [[False, True, False], OPT[i-3] - A[i-2] + A[i-1] - A[i]],
            [[False, False, False], OPT[i-3] - A[i-2] - A[i-1] - A[i]]],
            OPT, i, A
            )
            operations[i-2:i+1] = new_operations
            OPT[i-2:i+1] = sum 

    
    #backtracking

    return operations, OPT

A = [1, -2, -3, 10, -1, -2, -3, 10, 20, 30, 40, 20, -10]
A2 = [10, -1, -2, -3, -40, -50, -60, -70, -80, 5, 6, -7, -8, 9, -10, 15, -5, 4, -3, 20, -25, 30, -35, 40, -45, 5, -5, 5, -5, 10]

OPT = [0]* (len(A2))
operations = [None]* (len(A2))

flipped, vals = BuildOpt(A2, operations, OPT)
print(flipped)
print(vals)
n = len(A)



def find_optimal(A):
    n = len(A)
    OPT = [[[0 for _ in range(4)] for _ in range(2)] for _ in range(n)]  # n elements, 2 for keep/flip, 4 for consecutive counts (0 unused)
    for i in range(0, n):
        for k in [0, 1]:  # 0 for flip, 1 for keep
            for l in [1, 2, 3]:
                if l < 3:
                # Continue the same operation
                    OPT[i][k][l+1] = max(OPT[i][k][l+1], A[i] * (1 if k else -1) + OPT[i-1][k][l])
                # Switch the operation
                OPT[i][1-k][1] = max(OPT[i][1-k][1], A[i] * (-1 if k else 1) + OPT[i-1][k][l])
    # Initialize variables to track the maximum value and its corresponding k and l
    max_sum = float('-inf')
    optimal_k = -1
    optimal_l = -1

    # Iterate over the possible states for the last element 
    for k in [0, 1]:  # 0 for flip, 1 for keep
        for l in range(1, 4):  # 1 to 3 for consecutive counts
            # Check if the current state has a higher sum than the current maximum
            if OPT[n-1][k][l] > max_sum:
                max_sum = OPT[n-1][k][l]
                optimal_k = k
                optimal_l = l
    i = n - 1
    operation_sequence = ['keep' if optimal_k == 1 else 'flip']  # Last element decision

    while i > 0:
        prev_max_sum = float('-inf')
        prev_operation = -1
        prev_consecutive = -1

        # Explore both keeping and flipping for the previous step
        for k in [0, 1]:  # 0 for flip, 1 for keep
            for l in [1, 2, 3]:
                current_sum = OPT[i-1][k][l] + (A[i] * (1 if optimal_k == 1 else -1))
                # Find the maximum sum that leads to the current OPT value
                if current_sum > prev_max_sum and current_sum <= OPT[i][optimal_k][optimal_l]:
                    prev_max_sum = current_sum
                    prev_operation = k
                    prev_consecutive = l

        # Update for next iteration based on what we found
        optimal_k = prev_operation
        optimal_l = prev_consecutive
        operation_sequence.insert(0, 'keep' if optimal_k == 1 else 'flip')  # Prepend operation
        i -= 1

    return(operation_sequence, max_sum)

print(A2)
find_optimal(A2)