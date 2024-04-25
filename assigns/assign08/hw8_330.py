
def find_approximate_solution(a, b, c, D):
    # Initialize DP table
    M = [[0 for _ in range(D + 1)] for _ in range(4)]  # 4 because 3 items + 1 for base case

    # Fill the DP table
    for i in range(1, 4):  # 1 to 3 for a, b, c
        for d in range(1, D + 1):
            # Values to decide on whether to include the current item or not
            without_item = M[i-1][d]  # Maximum without current item
            with_item = 0
            
            # Determine the 'weight' of the current item based on i
            weight = a if i == 1 else b if i == 2 else c
            
            if weight <= d:  # Check if current item can be included
                with_item = weight + M[i][d-weight]
                
            M[i][d] = max(with_item, without_item)
    print(M)
    # M[3][D] now contains the maximum value achievable
    # To find x, y, z, backtrack from M[3][D]
    x, y, z = 0, 0, 0
    d = D
    vals = [0, a, b, c]
    while d > 0:
        for g in range(3, 0, -1):
            if M[3][d] == vals[g] + M[3][d-vals[g]]:
                if g == 1: x+=1 
                elif g == 2: y+=1 
                else: z+=1
                d -= vals[g]
                break
                
        
    
    return x, y, z, M[3][D]
# Example usage
a, b, c, D = 4, 6, 3, 200

print(find_approximate_solution(a, b, c, D))





