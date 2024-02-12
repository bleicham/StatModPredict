% Spatial wave loop: This code can be used to run multiple columns of 
% data at a time. The following code takes in inputs from the 'options.m'
% file and the information entered within this code to fit the spatial
% wave model to your data. 

% TO RUN (PLEASE READ): To run the following code, after setting your 
% options in the 'options.m' file, replace the X with the number of columns
% in your input data set. Next, you with replace the word DATE with the 
% same date entered for the `caddate1` parameter in the 'options.m' file.
% Please ensure that the date is in single quotes. Once that has been 
% completed, please hit the big green RUN button at the top of the page. 
% The toolbox will then proceed to determine the model fits for each 
% column of your data.

% Loop running model fits
for i = 1:X

    % Try-catch statement 
    try

        % Code call 
        Run_SW_subepidemicFramework(i,DATE)
    
    end % End of try-catch

end % End of loop 
