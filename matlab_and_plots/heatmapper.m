function heatmap = heatmapper(nodes, radius,shield,domain_x,domain_y)
%function to visualise radius of risk region for the geometry of the train
%based on people sitting at loctations `nodes` with a perimeter of social
%distancing `radius` and plastic blockades at locations `shields`. This is
%applied into `domain`, which assumes rectangular geometry of a train.
%works if nodes is a Nx2 matrix with first column the x position, second y
%position

%working example

theta = linspace(0,2*pi,100);
heatmap = zeros(2,100*length(nodes(:,1)));

for j = 1:length(nodes(:,1))
    
    x_circle = radius*cos(theta) + nodes(j,1);%Defines an intial unsafe perimeter
    y_circle = radius*sin(theta) + nodes(j,2);
    
    x_circle(x_circle<0) = 0;
    x_circle(x_circle>domain_x) = domain_x;
    y_circle(y_circle<0) = 0;
    y_circle(y_circle>domain_y) = domain_y;%Snaps unsafe perimeter to domain walls where necessary
    heatmap(1,1+100*(j-1):100*(j-1) + 100) = x_circle;
    heatmap(2,1+100*(j-1):100*(j-1) + 100) = y_circle;
    shield_interact = [];
    
    
    for i = 1:length(shield(:,1))
        shield_locations_y = linspace(shield(i,3),shield(i,4));
        shield_locations_x = repmat(shield(i,1), [1,100]);
        if min(min(sqrt((shield_locations_y-nodes(j,2)).^2 + (shield_locations_x-nodes(j,1)).^2)))<radius %checks shield interaction
            
            if shield_locations_x(1) < nodes(j,1) %barrier left of the node
                x_circle(x_circle < shield_locations_x & y_circle< max(shield_locations_y) & y_circle> min(shield_locations_y)) = shield_locations_x(x_circle < shield_locations_x & y_circle< max(shield_locations_y) & y_circle> min(shield_locations_y));
            else %barrier to right of node
                x_circle(x_circle > shield_locations_x & y_circle< max(shield_locations_y) & y_circle> min(shield_locations_y)) = shield_locations_x(x_circle > shield_locations_x & y_circle< max(shield_locations_y) & y_circle> min(shield_locations_y));
        
            end
            
            
            heatmap(1,1+100*(j-1):100*(j-1) + 100) = x_circle;
            heatmap(2,1+100*(j-1):100*(j-1) + 100) = y_circle;
        end
    end
    hold on
    plot(heatmap(1,1+100*(j-1):100*(j-1) + 100),heatmap(2,1+100*(j-1):100*(j-1) + 100),'-k')
    hold on
    patch('Faces',1:100,'Vertices',[heatmap(1,1+100*(j-1):100*(j-1) + 100) ;heatmap(2,1+100*(j-1):100*(j-1) + 100)]','FaceColor','red','FaceAlpha',.3)
end

end