"""
20-regression-mlp.py
--------------------
Simple tabular regression with a small MLP. Includes didactic code for:
- Model definition with one hidden layer
- Supervised training loop
- Example usage loading CSV data from pytorch/data
"""

import torch
import torch.nn as nn
from torch.utils.data import TensorDataset
import numpy as np
import torch.nn.functional as F
from pathlib import Path

class MLPNet(nn.Module):
    def __init__(self, in_size, hidden_size, out_size):
        super(MLPNet,self).__init__()
        # hidden layer
        self.linear1 = nn.Linear(in_size, hidden_size)
        # output layer
        self.linear2 = nn.Linear(hidden_size, out_size)
        
    def forward(self, xb):
        # Get intermediate outputs using hidden layer
        out = self.linear1(xb)
        # Apply activation function
        out = F.relu(out)
        # Get predictions using output layer
        out = self.linear2(out)
        return out     
        
        
def create_torch_mlp(input_size, hidden_size):
  device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
  model = MLPNet(in_size=int(input_size), hidden_size=int(hidden_size), out_size=1).to(device)
  return(model)      
      

def torch_fit_mlp(epochs, lr, model, train_loader, opt_func=torch.optim.SGD):
    # to track the training loss as the model trains
    train_losses = []
    # to track the validation loss as the model trains
    valid_losses = []
    # to track the average training loss per epoch as the model trains
    avg_train_losses = []
    # to track the average validation loss per epoch as the model trains
    avg_valid_losses = [] 
    
    criterion = nn.MSELoss()

    optimizer = opt_func(model.parameters(), lr)
    for epoch in range(epochs):
        # train the model #
        model.train() # prep model for training
        for data, target in train_loader:
            # clear the gradients of all optimized variables
            model.zero_grad()
            # forward pass: compute predicted outputs by passing inputs to the model
            output = model(data.float())

            #print('Going to compute loss...')
            # calculate the loss
            loss = criterion(output, target.float())

            #print('Done computing loss.')

            # backward pass: compute gradient of the loss with respect to model parameters
            loss.backward()
            # perform a single optimization step (parameter update)
            optimizer.step()
            # record training loss
            train_losses.append(loss.item())

        # validate the model #
        model.eval() # prep model for evaluation

        # print training/validation statistics 
        # calculate average loss over an epoch
        train_loss = np.average(train_losses)
        avg_train_losses.append(train_loss)

        # clear lists to track next epoch
        train_losses = []

    return  model, avg_train_losses


def train_torch_mlp(model, df_train, n_epochs = 3000, lr = 0.001, target = ""):
	X_train = df_train.drop(target, axis=1).to_numpy()
	y_train = df_train[target].to_numpy()

	X_train = X_train[:, :, np.newaxis]
	y_train = y_train[:, np.newaxis]
	
	train_x = torch.from_numpy(X_train)
	train_y = torch.from_numpy(y_train)
	
	train_x = train_x.squeeze()
	
	train_ds = TensorDataset(train_x, train_y)
	
	BATCH_SIZE = 8
	train_loader = torch.utils.data.DataLoader(train_ds, batch_size = BATCH_SIZE, shuffle = False)
	
	model = model.float()
	n_epochs = int(n_epochs)
	model, train_loss = torch_fit_mlp(n_epochs, lr, model, train_loader, opt_func=torch.optim.Adam)
	
	return model


def predict_torch_mlp(model, df_test, target):
  X_test = df_test.drop(target, axis=1).to_numpy()
  y_test = df_test[target].to_numpy()
  
  X_test = X_test[:, :, np.newaxis]
  y_test = y_test[:, np.newaxis]
  
  test_x = torch.from_numpy(X_test)
  test_y = torch.from_numpy(y_test)
  
  test_x = torch.squeeze(test_x, 2)

  test_ds = TensorDataset(test_x, test_y)
  
  BATCH_SIZE = 8
  test_loader = torch.utils.data.DataLoader(test_ds, batch_size = BATCH_SIZE, shuffle = False)
  
  outputs = []
  with torch.no_grad():
    for xb, yb in test_loader:
      output = model(xb.float())
      outputs.append(output)
  
  test_predictions = torch.vstack(outputs).squeeze(1)  
  test_predictions = test_predictions.numpy()
  
  return test_predictions
if __name__ == "__main__":
    # Example usage: load regression train/test and run a quick experiment
    import pandas as pd
    this_dir = Path(__file__).resolve().parent
    data_dir = this_dir / "data"

    df_train = pd.read_csv(data_dir / 'data_reg_train.csv')
    df_test = pd.read_csv(data_dir / 'data_reg_test.csv')

    TRAIN_MODE = True
    if TRAIN_MODE:
        model = create_torch_mlp(input_size=13, hidden_size=3)
        model = train_torch_mlp(model, df_train, n_epochs=3000, lr=0.001, target="medv")

    preds = predict_torch_mlp(model, df_test, target="medv")
    print(preds)

