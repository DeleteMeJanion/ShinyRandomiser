class Distribution:
  
  def __init__(self, samples):
    if samples is None:
      self.samples = []
    elif isinstance(samples, list):
      self.samples = samples
    else:
      self.samples = [samples]

    self.samples.sort()
  
  def calculate_quantiles(self, quantity):
    if len(self.samples) == 0:
      return []
    if len(self.samples) == 1:
      return [self.samples[0] for _ in range(quantity - 1)]
    
    quantile_width = (len(self.samples) - 1) / quantity
    
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
#   samples = [1, 2]
#   d = Distribution(samples)
#   for q in d.calculate_quantiles(4):
#     print(q)


