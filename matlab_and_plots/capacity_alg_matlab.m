close all
clc
clear


rad = linspace(0.1,2,20);
cap = [];

tic
for i = 1:numel(rad)
    cap(i) = capacity(rad(i));
end
toc



%plot of capacity
plot(rad,cap,'-k')
xlabel("Social distancing measure (m)",'Interpreter','latex')
ylabel("Capacity ($\%$)",'Interpreter','latex')
box off
xlim([rad(1),rad(end)])
title("TfW: Class 150 - Carriage 1",'Interpreter','latex')



seats = readtable('seat_locations150.csv');

seat_locs = table2array(seats(:,:));


%algorithm to find max capicity 

                     
 max_num_of_seats = numel(seat_locs(:,1));
 accepted_seats = [];
 radius  = 2; % change this for node plots !!!%
 
for i = 1:max_num_of_seats
    accepted_seats(i) = i;
end

  
 for i = 1:numel(accepted_seats)
      
     num_to_remove = []; 
    
     if i <= numel(accepted_seats)
         
         %go through each accepted node and determine if too close
         
         for m = accepted_seats(i):accepted_seats(end)
             
             fixed_seat = seat_locs(accepted_seats(i),:);
             trial_seat = seat_locs(m,:);
                        
             %if too close then a seat number to list 
             
             if (norm ([fixed_seat(1),fixed_seat(2)] - [trial_seat(1),trial_seat(2)] ) < radius && isequal(fixed_seat,trial_seat) == 0 )
                 
                 num_to_remove = [num_to_remove, m];
      
             end
             
         end
         
         
         %remove the nodes too close from the accepted list
         
         for j = 1:numel(accepted_seats)
             for k = 1:numel(num_to_remove)
                 
                 if j  <= numel(accepted_seats)
                     
                     if accepted_seats(j) == num_to_remove(k)
                         
                         accepted_seats = accepted_seats(accepted_seats~=num_to_remove(k));
                         
                     end
                 end
             end
         end   
         
     end
     
 end
 
 
 nodes_for_heatmapper = [];
for i  = 1:numel(accepted_seats)
    nodes_for_heatmapper(i,1) = seat_locs(accepted_seats(i),1);
    nodes_for_heatmapper(i,2) = seat_locs(accepted_seats(i),2);
end
 
 
 


out = (numel(accepted_seats))


length = 17.5;
  disp(["The current operating capacity is " (numel(accepted_seats)./max_num_of_seats)*100 "% when oberserving social distancing at "  radius "m"])
  
  figure()
  scatter(seat_locs(:,1),seat_locs(:,2),50,'xk','linewidth',2 )
  hold on
  for i = 1:numel(accepted_seats)
   scatter(seat_locs(accepted_seats(i),1),seat_locs(accepted_seats(i),2),500,'.b' ) 
  end
  heatmapper(nodes_for_heatmapper, radius,[0,0,0,0],length,2.816);
  plot([0,0],[0,2.816],'-k')
  plot([0,length],[2.816,2.816],'-k')
  plot([length,length],[2.816,0],'-k')
  plot([0,length],[0,0],'-k')
  xlabel("$x$ (m)",'interpreter','latex');
  ylabel("$y$ (m)",'interpreter','latex')
  axis equal
  axis off
  
  



function out = capacity(radius)

seats = readtable('seat_locations150.csv');

seat_locs = table2array(seats(:,:));


%algorithm to find max capicity 

                     
 max_num_of_seats = numel(seat_locs(:,1));
 accepted_seats = [];
 
 
for i = 1:max_num_of_seats
    accepted_seats(i) = i;
end

  
 for i = 1:numel(accepted_seats)
      
     num_to_remove = []; 
    
     if i <= numel(accepted_seats)
         
         %go through each accepted node and determine if too close
         
         for m = accepted_seats(i):accepted_seats(end)
             
             fixed_seat = seat_locs(accepted_seats(i),:);
             trial_seat = seat_locs(m,:);
                        
             %if too close then a seat number to list 
             
             if (norm ([fixed_seat(1),fixed_seat(2)] - [trial_seat(1),trial_seat(2)] ) < radius && isequal(fixed_seat,trial_seat) == 0 )
                 
                 num_to_remove = [num_to_remove, m];
      
             end
             
         end
         
         
         %remove the nodes too close from the accepted list
         
         for j = 1:numel(accepted_seats)
             for k = 1:numel(num_to_remove)
                 
                 if j  <= numel(accepted_seats)
                     
                     if accepted_seats(j) == num_to_remove(k)
                         
                         accepted_seats = accepted_seats(accepted_seats~=num_to_remove(k));
                         
                     end
                 end
             end
         end   
         
     end
     
 end
 
 
%  nodes_for_heatmapper = [];
% for i  = 1:numel(accepted_seats)
%     nodes_for_heatmapper(i,1) = seat_locs(accepted_seats(i),1);
%     nodes_for_heatmapper(i,2) = seat_locs(accepted_seats(i),2);
% end
%  
 
 


out = (numel(accepted_seats)./max_num_of_seats)*100;
end


 
 
%   disp(["The current operating capacity is " (numel(accepted_seats)./max_num_of_seats)*100 "% when oberserving social distancing at "  radius "m"])
%   
%   figure()
%   scatter(seat_locs(:,1),seat_locs(:,2),50,'xk','linewidth',2 )
%   hold on
%   for i = 1:numel(accepted_seats)
%    scatter(seat_locs(accepted_seats(i),1),seat_locs(accepted_seats(i),2),500,'.r' ) 
%   end
%   heatmapper(nodes_for_heatmapper, radius,[0,0,0,0],70,200);


 