"""
50-autoencoder.py
-----------------
Basic dense autoencoder for time series windows.
Provides helper functions to create, train, and use the autoencoder
from R via reticulate or directly in Python.
"""

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import numpy as np
import pandas as pd


class AutoEncoderTS(Dataset):
    """Tiny dataset wrapper for autoencoder training on windowed time series."""

    def __init__(self, data):
        # data is expected as (n_samples, window_size, 1)
        self.data = data

    def __len__(self):
        return self.data.shape[0]

    def __getitem__(self, index):
        return self.data[index], self.data[index]

class Autoencoder(nn.Module):
    def __init__(self, input_size, encoding_size):
        super(Autoencoder, self).__init__()

        self.encoder = nn.Sequential(
            nn.Linear(input_size, 64),
            nn.ReLU(True),
            nn.Linear(64, encoding_size))

        self.decoder = nn.Sequential(
            nn.Linear(encoding_size, 64),
            nn.ReLU(True),
            nn.Linear(64, input_size))

    def forward(self, x):
        x = self.encoder(x)
        x = self.decoder(x)
        return x
    
# Create the autoencoder
def autoencoder_create(input_size, encoding_size):
  input_size = int(input_size)
  encoding_size = int(encoding_size)
  
  autoencoder = Autoencoder(input_size, encoding_size)
  autoencoder = autoencoder.float()
  return autoencoder  


# Train the autoencoder
def autoencoder_train(autoencoder, train_loader, num_epochs = 1000, learning_rate = 0.001):
  criterion = nn.MSELoss()
  optimizer = optim.Adam(autoencoder.parameters(), lr=learning_rate)

  for epoch in range(num_epochs):
      running_loss = 0.0
      for data in train_loader:
          inputs, _ = data
          inputs = inputs.float()
          inputs = inputs.view(inputs.size(0), -1)
          optimizer.zero_grad()
          outputs = autoencoder(inputs)
          loss = criterion(outputs, inputs)
          loss.backward()
          optimizer.step()
          running_loss += loss.item()
        # Uncomment for periodic logging
        # if (epoch + 1) % 100 == 0:
        #     print(f"Epoch {epoch+1} Loss: {running_loss/len(train_loader):.4f}")

  return autoencoder

def autoencoder_fit(autoencoder, data, batch_size = 32, num_epochs = 1000, learning_rate = 0.001):
  batch_size = int(batch_size)
  num_epochs = int(num_epochs)
  
  array = data.to_numpy()
  array = array[:, :, np.newaxis]
  
  ds = AutoEncoderTS(array)
  train_loader = DataLoader(ds, batch_size=batch_size)
  
  autoencoder = autoencoder_train(autoencoder, train_loader, num_epochs = num_epochs, learning_rate = learning_rate)
  
  return autoencoder



def encode_data(autoencoder, data_loader):
  # Encode the synthetic time series data using the trained autoencoder
  encoded_data = []
  for data in data_loader:
      inputs, _ = data
      inputs = inputs.float()
      inputs = inputs.view(inputs.size(0), -1)
      encoded = autoencoder.encoder(inputs)
      encoded_data.append(encoded.detach().numpy())

  encoded_data = np.concatenate(encoded_data, axis=0)

  return encoded_data

def autoencoder_encode(autoencoder, data, batch_size = 32):
  array = data.to_numpy()
  array = array[:, :, np.newaxis]
  
  ds = AutoEncoderTS(array)
  train_loader = DataLoader(ds, batch_size=batch_size)
  
  encoded_data = encode_data(autoencoder, train_loader)
  
  return(encoded_data)


def encode_decode_data(autoencoder, data_loader):
  # Encode the synthetic time series data using the trained autoencoder
  encoded_decoded_data = []
  for data in data_loader:
      inputs, _ = data
      inputs = inputs.float()
      inputs = inputs.view(inputs.size(0), -1)
      encoded = autoencoder.encoder(inputs)
      decoded = autoencoder.decoder(encoded)
      encoded_decoded_data.append(decoded.detach().numpy())

  encoded_decoded_data = np.concatenate(encoded_decoded_data, axis=0)

  return encoded_decoded_data


def autoencoder_encode_decode(autoencoder, data, batch_size = 32):
  array = data.to_numpy()
  array = array[:, :, np.newaxis]
  
  ds = AutoEncoderTS(array)
  train_loader = DataLoader(ds, batch_size=batch_size)
  
  encoded_decoded_data = encode_decode_data(autoencoder, train_loader)
  
  return(encoded_decoded_data)
  
