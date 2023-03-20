import numpy as np
from sklearn.linear_model import LinearRegression
import matplotlib.pyplot as plt

# PTMC Mn values
ptmc_mn = np.array([1.7, 1.7, 1.7, 1.7, 1.7, 2.6, 2.6, 2.6, 2.6, 2.6])

# Volume fraction values
volume_fraction = np.array([0.041, 0.050, 0.054, 0.068, 0.070, 0.041, 0.045, 0.051, 0.058, 0.061])

# Modulus values
modulus = np.array([51, 74, 94, 141, 150, 30, 32, 42, 53, 61])

# Plot the data
plt.scatter(volume_fraction, modulus)

# Fit a linear regression model
regressor = LinearRegression()
regressor.fit(volume_fraction.reshape(-1,1), modulus)
modulus_predicted = regressor.predict(volume_fraction.reshape(-1,1))

# Plot the regression line
plt.plot(volume_fraction, modulus_predicted, color='red')

# Add labels and title
plt.xlabel('Volume fraction')
plt.ylabel('Modulus (kPa)')
plt.title('Modulus vs Volume Fraction')

# Show the plot
plt.show()
