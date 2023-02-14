class Distribution:
  
  def __init__(self, samples):
    self.samples = samples
  
  def calculate_quantiles(self, quantity):
    quantile_width = len(self.samples) / quantity
    self.samples.sort()
    
    quantiles = []
    for i in range(1, quantity):
      quantile_location = i * quantile_width
      index = int(quantile_location)
      interpolation = quantile_location % 1
      
      lower = self.samples[index]
      upper = self.samples[index + 1]
      
      quantiles.append(lower + (upper - lower) * interpolation)
      
    return quantiles

# if __name__ == '__main__':
#   import random
#   samples = [random.gauss(0, 1) for _ in range(1000000)]
#   d = Distribution(samples)
#   for q in d.calculate_quantiles(4):
#     print(q)


