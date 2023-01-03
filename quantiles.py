class Distribution:
  
  def __init__(self, samples):
    self.samples = samples
  
  def calculate_quantiles(self, quantity):
    percentile_width = len(self.samples) / quantity
    self.samples.sort()
    
    percentiles = []
    for i in range(1, quantity):
      percentile_location = i * percentile_width
      index = int(percentile_location)
      interpolation = percentile_location % 1
      
      lower = self.samples[index]
      upper = self.samples[index + 1]
      
      percentiles.append(lower + (upper - lower) * interpolation)
      
    return percentiles

# if __name__ == '__main__':
#   import random
#   samples = [random.gauss(0, 1) for _ in range(1000000)]
#   d = Distribution(samples)
#   for q in d.calculate_quantiles(4):
#     print(q)


