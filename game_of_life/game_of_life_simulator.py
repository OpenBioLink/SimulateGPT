"""
I want you to write a python program that takes as input a grid of alive and dead cells from a "conway's game of life simulation" and simulates the output:

01000100
11000010
00111000
11011011
00100100
11111111
00000000
01110001
"""


def create_grid(input_str):
    grid = []

    for line in input_str.split("\n"):
        grid.append([int(cell) for cell in line])

    return grid


def count_alive_neighbors(grid, x, y):
    alive_neighbors = 0
    for i in range(-1, 2):
        for j in range(-1, 2):
            if i == 0 and j == 0:
                continue
            nx, ny = x + i, y + j
            if nx >= 0 and ny >= 0 and nx < len(grid) and ny < len(grid[0]):
                alive_neighbors += grid[nx][ny]

    return alive_neighbors


def simulate(grid):
    new_grid = [[0 for _ in range(len(grid[0]))] for _ in range(len(grid))]

    for x in range(len(grid)):
        for y in range(len(grid[0])):
            alive_neighbors = count_alive_neighbors(grid, x, y)

            if grid[x][y]:
                if alive_neighbors == 2 or alive_neighbors == 3:
                    new_grid[x][y] = 1

                else:
                    new_grid[x][y] = 0
            else:
                if alive_neighbors == 3:
                    new_grid[x][y] = 1

    return new_grid


def print_grid(grid):
    for row in grid:
        print("".join(str(cell) for cell in row))

input_5x5 = """01000
00100
11100
00000
00000"""

input_4x4 = """0100
0010
1110
0000"""

if __name__ == "__main__":
    input_str = input_4x4

    grid = create_grid(input_str)
    initial_grid = grid
    print("Initial grid:")
    print_grid(grid)
    for i in range(4):
        print(f"Generation {i+1}")
        grid = simulate(grid)
        print("```")
        print_grid(grid)
        print("```\n")
