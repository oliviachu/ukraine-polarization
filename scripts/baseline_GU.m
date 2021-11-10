% Geographically uninformed model.

% Order of the data: 
%   -respondent ID number
%   -eu_support_12
%   -extraversion imputed
%   -agreeableness imputed
%   -political discussion 

% mean(ukraine_data$agreeableness_imp)
% [1] 0.62187

function[A_end,op_end] = baseline_GU(G,dat,phi)

current_opinion = dat(:,2);

num_resp = size(G,1);

freq = 50000;

for i=1:200000
    % Take snapshots. 
        if (~mod(i,freq)==1)
            save(sprintf('A_%d.mat',i),'G');
            save(sprintf('op_%d.mat',i),'current_opinion');
        end  
    % Choose random individual in the population. 
    chosen_ind = randi(num_resp);
    G_tmp = graph(G);
    prob = rand();
    if (degree(G_tmp,chosen_ind) == 0)
        continue;
    end
    %With probability phi (times the avg. agreeableness), do social learning
     if (prob < phi)
         rand_sl = rand();
         if (rand_sl < 0.62187)
        neighbors_ind = neighbors(G_tmp,chosen_ind);
        random_neighbor_tmp = randi(size(neighbors_ind,1));
        random_neighbor = neighbors_ind(random_neighbor_tmp);
        current_opinion(chosen_ind) = current_opinion(random_neighbor); 
         end
     end
    %With probability 1-phi, do homophily
     if (prob >= phi) 
        neighbors_ind = neighbors(G_tmp,chosen_ind);
        random_neighbor_tmp = randi(size(neighbors_ind,1));
        random_neighbor_cut = neighbors_ind(random_neighbor_tmp);
        cur_op = current_opinion(chosen_ind);
        others_w_op = find(current_opinion == cur_op);
        others_less_ind = others_w_op(others_w_op~=chosen_ind);
          if (isempty(others_less_ind))
            continue;
          end
          if (~isempty(others_less_ind))
            G(chosen_ind,random_neighbor_cut) = 0;
            G(random_neighbor_cut,chosen_ind) = 0; 
            chosen_same = randi(length(others_less_ind));
            G(chosen_ind,others_less_ind(chosen_same)) = 1;
            G(others_less_ind(chosen_same),chosen_ind) = 1; 
          end
     end  
end

A_end = G;
op_end = current_opinion;

% Visualize networks. 

%op0_end = find(current_opinion == 0);
%op1_end = find(current_opinion == 1);
%op2_end = find(current_opinion == 2);

%figure()

%G_end = graph(G);
%p_end = plot(G_end,'NodeLabel',{});

%highlight(p_end,op0_end,'NodeColor',[0.4660, 0.6740, 0.1880]);

%highlight(p_end,op1_end,'NodeColor',[0, 0.4470, 0.7410]);

%highlight(p_end,op2_end,'NodeColor',[0.75, 0, 0.75]);

%set(gca,'XColor', 'none','YColor','none')
%p_end.MarkerSize = 12;
%p_end.EdgeColor = 'black';
%p_end.LineWidth = 1.5;

end
