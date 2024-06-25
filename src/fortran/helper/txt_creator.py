import numpy as np

def generate_data(num_lines=100):
    mass = 1e11
    data = []

    for _ in range(num_lines):
        position = np.random.uniform(-10, 10, 3)
        velocity = np.random.uniform(-1, 1, 3)
        acceleration = np.random.uniform(-0.1, 0.1, 3)
        line = f"{mass:.1f} {position[0]:.3f} {position[1]:.3f} {position[2]:.3f} {velocity[0]:.3f} {velocity[1]:.3f} {velocity[2]:.3f} {acceleration[0]:.3f} {acceleration[1]:.3f} {acceleration[2]:.3f}"
        data.append(line)

    return data

def save_to_file(data, filename):
    with open(filename, 'w') as f:
        f.write("# Mass, Position (x, y, z), Velocity (vx, vy, vz), Acceleration (ax, ay, az)\n")
        for line in data:
            f.write(line + "\n")

# Generate 100 lines of data
data = generate_data(1000000)

# Save the data to a file
filename = "1000000obj.txt"
save_to_file(data, filename)

print(f"Data successfully written to {filename}")


#100000000000.0 4.412408968496397 1.1770013960272543 4.382598055841472 0.5523604261716812 0.9789271845331913 -0.6831777696284371 0.0234722177268675 0.048570149285735165 -0.07961834775923078
#1e11 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0