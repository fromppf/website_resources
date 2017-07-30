from scipy import ndimage
from scipy import misc
import numpy as np
import torch
from sub import subMNIST  
from torchvision import datasets, transforms
from scipy.ndimage.interpolation import map_coordinates
from scipy.ndimage.filters import gaussian_filter

# Author Varun : https://www.linkedin.com/in/varundn

def rotation(x, angle):
    ''' 
    This function rotates a given image by a given angle and put's it back into it's o
    
    inal size.
    
    INPUT: x (2-D image - numpy array), angle (angle - float value)
    RETURNS: Rotated image (2-D numpy array)    
    '''

    return ndimage.rotate(x, angle, reshape=False)


def scale(x, sf):
    '''
    This function scales a given image and put's it back into it's original size.
    
    INPUT: x (2-D image - numpy array), sf (scaling factor - float value)
    RETURNS: Rescaled image (2-D numpy array)
    '''
    
    x_resize = misc.imresize(x, sf)
    
    if sf <= 1.:
        X = x.min() * np.ones(x.shape)

        X[(x.shape[0] - x_resize.shape[0]) / 2 : x_resize.shape[0] + (x.shape[0] - x_resize.shape[0]) / 2, 
          (x.shape[0] - x_resize.shape[0]) / 2 : x_resize.shape[0] + (x.shape[0] - x_resize.shape[0]) / 2] = x_resize
    
        return X
    
    if (sf * 10.) % 2 == 1:
        return x_resize[(x_resize.shape[0] - x.shape[0]) / 2: x_resize.shape[0] - (x_resize.shape[0] - x.shape[0]) / 2,
            (x_resize.shape[0] - x.shape[0]) / 2: x_resize.shape[0] - (x_resize.shape[0] - x.shape[0]) / 2]
    
    return x_resize[(x_resize.shape[0] - x.shape[0]) / 2: x_resize.shape[0] - (x_resize.shape[0] - x.shape[0]) / 2 - 1,
            (x_resize.shape[0] - x.shape[0]) / 2: x_resize.shape[0] - (x_resize.shape[0] - x.shape[0]) / 2 - 1]



def translation(x, df, horizontal = True, direction = 'right'):
    '''
    This function translates a given image and put's it back into it's original size.
    
    INPUT: x (2-D image - numpy array), 
           df (distance factor - float value), 
           horizontal (whether the translation is horizontal or vertical),
           direction (right, left, up, down - use these options in the right context)
           
    RETURNS: Translated image (2-D numpy array)    
    
    '''

    if df > 1.:
        df = df - int(df)

    distance = int(x.shape[0] * df)
    
    if horizontal and direction == 'right':
        return np.hstack((x.min() * np.ones((x.shape[0], distance)), x[:, :x.shape[0] - distance]))
    
    if horizontal and direction == 'left':
        return np.hstack((x[:, distance:], x.min() * np.ones((x.shape[0], distance))))
    
    if not horizontal and direction == 'down':
        return np.vstack((x.min() * np.ones((distance, x.shape[0])), x[:x.shape[0] - distance, :]))
    
    return np.vstack((x[distance:, :], x.min() * np.ones((distance, x.shape[0]))))

def elastic_transform(image, alpha, sigma, random_state=None):
    """Elastic deformation of images as described in [Simard2003]_.
    .. [Simard2003] Simard, Steinkraus and Platt, "Best Practices for
       Convolutional Neural Networks applied to Visual Document Analysis", in
       Proc. of the International Conference on Document Analysis and
       Recognition, 2003.
    """
    if random_state is None:
        random_state = np.random.RandomState(None)

    shape = image.shape
    dx = gaussian_filter((random_state.rand(*shape) * 2 - 1), sigma, mode="constant", cval=0) * alpha
    dy = gaussian_filter((random_state.rand(*shape) * 2 - 1), sigma, mode="constant", cval=0) * alpha

    x, y = np.meshgrid(np.arange(shape[0]), np.arange(shape[1]))
    indices = np.reshape(y+dy, (-1, 1)), np.reshape(x+dx, (-1, 1))

    return map_coordinates(image, indices, order=1).reshape(shape)

def all_transformations(dataset_pickle, data_type, type_transformation=['rotation'],value_rotation=45.0,
                        value_scale=0.8,distance_translation=0.1,direction_translation = 'right',horizontal_translation=True,
                        elastic_alpha = 34, elastic_sigma = 4):
    '''
    This function takes the pickle data (train and test) and pre-process with 3 different data-augmentation techniques.
    We can change default values of each transformation.
    
    INPUT: dataset_pickle (pickle pytorch), 
           data_type ='train' or 'train_unlabel' or 'test',
           type_transformation=['rotation'], ['scale'], ['translation'], combinations (i.e. ['rotation','scale']) or ['all'], 
           
    RETURNS: Transformed data, ready to load with torch.utils.data.DataLoader
    
    '''
    transform=transforms.Compose([transforms.ToTensor(),
                              transforms.Normalize((0.1307,), (0.3081,))
                             ])
    if data_type == 'train' :
        dataset_loader_numpy = dataset_pickle.train_data.numpy()
        dataset_label_numpy = dataset_pickle.train_labels.numpy()
        dataset_import = subMNIST(root='./data', train=True, transform = transform ,
                                  download=True, k=dataset_pickle.train_data.size()[0])
    if data_type == 'train_unlabel' :
        dataset_loader_numpy = dataset_pickle.train_data.numpy()
        dataset_label_numpy = dataset_pickle.train_labels.numpy()
        dataset_import = subMNIST(root='./data', train=True, transform = transform ,
                                  download=True, k=dataset_pickle.train_data.size()[0])

    if type_transformation == ['rotation']:
        dataset_loader_numpy_transformed = np.array( [rotation(x,value_rotation) for x in dataset_loader_numpy] )
    if type_transformation == ['elastic']:
        dataset_loader_numpy_transformed = np.array( [elastic_transform(x,elastic_alpha, elastic_sigma) for x in dataset_loader_numpy] )
    if type_transformation == ['scale']:
        dataset_loader_numpy_transformed = np.array( [scale(x,value_scale) for x in dataset_loader_numpy] )
    if type_transformation == ['translation']:
        dataset_loader_numpy_transformed = np.array( [translation(x,distance_translation,direction=direction_translation,horizontal=horizontal_translation) for x in dataset_loader_numpy] )
    if type_transformation == ['rotation','scale'] or type_transformation == ['scale','rotation']:
        dataset_loader_numpy_transformed = np.array( [rotation(x,value_rotation) for x in dataset_loader_numpy] )
        dataset_loader_numpy_transformed = np.array( [scale(x,value_scale) for x in dataset_loader_numpy_transformed] )
    if type_transformation == ['rotation','translation'] or type_transformation == ['translation','rotation']:
        dataset_loader_numpy_transformed = np.array( [rotation(x,value_rotation) for x in dataset_loader_numpy] )
        dataset_loader_numpy_transformed = np.array( [translation(x,distance_translation,direction=direction_translation,horizontal=horizontal_translation) for x in dataset_loader_numpy_transformed] )
    if type_transformation == ['scale','translation'] or type_transformation == ['translation','scale']:
        dataset_loader_numpy_transformed = np.array( [scale(x,value_scale)  for x in dataset_loader_numpy] )
        dataset_loader_numpy_transformed = np.array( [translation(x,distance_translation,direction=direction_translation,horizontal=horizontal_translation) for x in dataset_loader_numpy_transformed] )
    if type_transformation == ['rotation', 'scale', 'translation','elastic']:
        dataset_loader_numpy_transformed = np.array( [rotation(x,value_rotation) for x in dataset_loader_numpy] )
        dataset_loader_numpy_transformed = np.array( [scale(x,value_scale)  for x in dataset_loader_numpy_transformed] )
        dataset_loader_numpy_transformed = np.array( [translation(x,distance_translation,direction=direction_translation,horizontal=horizontal_translation) for x in dataset_loader_numpy_transformed] )
        dataset_loader_numpy_transformed = np.array(  [elastic_transform(x,elastic_alpha, elastic_sigma) for x in dataset_loader_numpy_transformed]  )
    if type_transformation == ['rotation','elastic']:
        dataset_loader_numpy_transformed = np.array( [rotation(x,value_rotation) for x in dataset_loader_numpy] )
        dataset_loader_numpy_transformed = np.array(  [elastic_transform(x,elastic_alpha, elastic_sigma) for x in dataset_loader_numpy_transformed]  )
         

    if type_transformation == ['all']:
        dataset_loader_numpy_transformed = np.array( [rotation(x,value_rotation) for x in dataset_loader_numpy] )
        dataset_loader_numpy_transformed = np.array( [scale(x,value_scale)  for x in dataset_loader_numpy_transformed] )
        dataset_loader_numpy_transformed = np.array( [translation(x,distance_translation,direction=direction_translation,horizontal=horizontal_translation) for x in dataset_loader_numpy_transformed] )
    
    dataset_loader_preprocessed = torch.from_numpy(dataset_loader_numpy_transformed)
    dataset_loader_preprocessed2 = dataset_loader_preprocessed.type(torch.ByteTensor)
    
    if data_type == 'train' :
        print("TRAIN TYPE")
        dataset_import.train_data = dataset_loader_preprocessed2.clone()
        dataset_import.train_labels = torch.from_numpy(dataset_label_numpy).clone()
    if data_type == 'train_unlabel' :
        dataset_import.train_data = dataset_loader_preprocessed2.clone()
        dataset_import.train_labels = torch.from_numpy(np.repeat(-1,dataset_import.train_data.size()[0])).clone()

    return dataset_import



def join_MNIST_tensors(in1, in2):
    transform=transforms.Compose([transforms.ToTensor(),
                              transforms.Normalize((0.1307,), (0.3081,))
                             ])
                                      
    joint_data = torch.cat((in1.train_data, in2.train_data),0)
    joint_labels = torch.cat((in1.train_labels, in2.train_labels),0)
    
    joint_total = subMNIST(root='./data', train=True, download=True, transform = transform,
                           k=in1.train_data.size()[0]+ in2.train_data.size()[0])
    joint_total.train_data = joint_data.clone()
    joint_total.train_labels = joint_labels.clone()
    
    return joint_total





