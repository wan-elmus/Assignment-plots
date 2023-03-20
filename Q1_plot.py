import numpy as np
import matplotlib.pyplot as plt

# define the variables
rho = 1.3 # density of the elastomer
N = 6.02214076e23 # Avogadro's number
A = 1 # cross-sectional area of the sample (arbitrary unit)
kB = 1.380649e-23 # Boltzmann constant
T = 298 # temperature in Kelvin
L = 1e-6 # contour length of the polymer (arbitrary unit)

# load the data from the table
epsilon = np.array([0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.31, 0.51, 0.71, 0.91, 1.11, 1.31, 1.51, 1.71, 1.91, 2.11, 2.31])
sigma = np.array([0.00, 0.03, 0.06, 0.09, 0.11, 0.14, 0.17, 0.19, 0.22, 0.24, 0.27, 0.29, 0.72, 1.08, 1.40, 1.60, 1.80, 2.10, 2.50, 3.00, 3.60, 4.10, 4.70])

# plot the stress-strain curve
plt.plot(epsilon, sigma, 'o-')
plt.xlabel('Strain (ε)')
plt.ylabel('Stress (σ) (MPa)')
plt.title('Stress-Strain Curve of Cross-linked Silicone Elastomer')
plt.show()

# find the maximum stress at the end of the linear region
linear_region = np.where(epsilon <= 0.1)[0] # indices of the data in the linear region (ε <= 0.1)
max_stress = np.max(sigma[linear_region]) # maximum stress in the linear region
print('Maximum stress in the linear region:', max_stress, 'MPa')
