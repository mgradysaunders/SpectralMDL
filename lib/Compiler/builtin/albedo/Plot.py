import numpy as np
from matplotlib import pyplot as plt

fig, ax = plt.subplots(nrows=1, ncols=1, sharex=False, sharey=False, figsize=(8,8))
#ax.set_title('Title')
#ax.set_xlim([-1, 1])
#ax.set_ylim([-1, 1])
#ax.set_xticks([])
#ax.set_yticks([])
#ax.set_xticklabels([])
#ax.set_yticklabels([])
im = np.loadtxt('simple_glossy_bsdf.txt')
plt.imshow(im)
plt.show()
