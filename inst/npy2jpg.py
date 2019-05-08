import os
import os.path as path
import numpy as np
from tqdm import tqdm
from PIL import Image

# FS
DIR_IN = "V:radiology/msh_hip/00_parsed_dcms"

#%% Locate image files
fs = os.listdir(IMG_DIR)
img_fs = list(filter(lambda fn: fn.endswith(".npy"), fs))
img_seqs = list(map(lambda fn: fn[:-4], img_fs))

seq2path = lambda img_seq: path.join(IMG_DIR, seq + '.npy')

#%% Generate Model
inception_base = InceptionV3(include_top=False, weights='imagenet')
btl_nck = GlobalAveragePooling2D()(inception_base.output)
btl_nck_net = Model(inputs = inception_base.input, outputs=btl_nck)

#%% Engine

# product container
feat_colnms = [".".join(["btlnck", str(n)]) for n in range(2048)]
btlnck_df = pd.DataFrame(index = img_seqs, columns = feat_colnms)

# loop by batch
batch_size = 32
n_samples = len(img_seqs)

seq = img_seqs[0]

img_np = np.load(seq2path(seq))
img = Image.fromarray(img_np[0, :, :])
rgb = img.convert('RGB')  # convert to 3 channel to fit pretrained inception
img = np.array(rgb)


#%%
a = 3
def f(x):
    x = x + 1
    return("Returned String")

f(a)

class Radiograph(object):
    anatomic_location="cxr"
    def __init__(self, image_id, image_fp=None):
        self.image_id = image_id

    def load_img("dataset", "img_id"):


    pass

# Attribute lookup
Radiograph("nih_201_2").image_id