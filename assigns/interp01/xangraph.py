import matplotlib.pyplot as plt
import numpy as np

# Define the interest rate range
interest_rates = np.linspace(0.01, 0.1, 100)

# IS curve: Y = C(I) + I(I) + G + NX(I)
# Assume it shifts downwards with higher interest rates
output_IS = 500 - 4000 * interest_rates

# LM curve: M/P = L(Y, I)
# It shifts upwards with higher output levels for a given money supply and price level
output_LM = 300 + 3000 * interest_rates

# BP curve: depends on interest rate and exchange rate regime
# Fixed exchange rate might see BP curve very elastic or horizontal at the equilibrium interest rate
output_BP_fixed = np.full_like(interest_rates, 400)  # Fixed regime
output_BP_floating = 450 - 3500 * interest_rates  # Floating regime, more responsive to interest rate changes

plt.figure(figsize=(10, 7))

# Plotting the curves
plt.plot(output_IS, interest_rates, label='IS Curve')
plt.plot(output_LM, interest_rates, label='LM Curve')
plt.plot(output_BP_fixed, interest_rates, linestyle='--', label='BP Curve (Fixed Exchange Rate)')
plt.plot(output_BP_floating, interest_rates, linestyle='-.', label='BP Curve (Floating Exchange Rate)')

plt.title('Mundell-Fleming Model (IS-LM-BP Diagram)')
plt.xlabel('Output (Y)')
plt.ylabel('Interest Rate (i)')
plt.legend()
plt.grid(True)
plt.show()
