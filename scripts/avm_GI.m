%Geographically informed model.

% Order of the data: 
%   -respondent ID number
%   -eu_support_12
%   -extraversion imputed
%   -agreeableness imputed
%   -political discussion 

% mean(ukraine_data$agreeableness_imp)
% [1] 0.62187


function[A_end,op_end,corr_ts,stay_ts,move_ts] = avm_GI(G,dat,dists,p_sl,p_h,phi)

current_opinion = dat(:,2);

closest = dists;

num_resp = size(G,1);

freq = 50000;

corr_ts = zeros(1,150000);
stay_ts = zeros(1,150000);
move_ts = zeros(1,150000);

for i=1:200000
    % Take snapshots. 
        if (~mod(i,freq)==1)
            save(sprintf('A_%d.mat',i),'G');
            save(sprintf('op_%d.mat',i),'current_opinion');
        end     
    % Choose a random edge. 
    G_tmp = graph(G);
    edgelist = G_tmp.Edges;
    randedge = randi(size(edgelist,1));
    chosen_edge = edgelist(randedge,1);
    active = chosen_edge.(1);
    child = active(1);
    parent = active(2);
    prob = rand();
    % With probability phi (times p_SL), do social
    % learning (using graded opinion change). 
     if (prob < phi)
         rand_sl = rand();
         if (rand_sl < p_sl)
            if (current_opinion(child) == current_opinion(parent))
                continue;
            end
            if (current_opinion(parent) == 0 && current_opinion(child) == 1)
                current_opinion(child) = 0;
            end
            if (current_opinion(parent) == 0 && current_opinion(child) == 2)
                current_opinion(child) = 1;
            end
            if (current_opinion(parent) == 1)
                current_opinion(child) = 1;
            end
            if (current_opinion(parent) == 2 && current_opinion(child) == 0)
                current_opinion(child) = 1;
            end
            if (current_opinion(parent) == 2 && current_opinion(child) == 1)
                current_opinion(child) = 2;
            end
         end
     end
    % With probability 1-phi, do homophily (with probability p_H if the two
    % individuals have a different opinion). 
     if (prob >= phi) 
        if (current_opinion(child) == current_opinion(parent))
            continue;
        end
        if (current_opinion(parent) ~= current_opinion(child))
            rand_h = rand();
            if (rand_h < p_h)
                continue;
            end
            % Attempt to connect to a geographically close neighbor with
            % the same opinion (to whom you are not already connected). If
            % this is not possible, second individual in edge pair will
            % attempt this, and if this is not possible, a randomly chosen
            % individual will attempt this (to not have excessive edge
            % loss). 
            if (rand_h >= p_h)
                neighbors_ind = neighbors(G_tmp,child);
                G(child,parent) = 0;
                G(parent,child) = 0;
                near = closest(child,:);
                random_neighbor_cut = parent;
                near_already_conn = intersect(near,neighbors_ind);
                near_notconn = setdiff(near,near_already_conn);
                nearest_same = find(current_opinion(near_notconn) == current_opinion(child));
                nearest_same_size = length(nearest_same);
                if (nearest_same_size ~= 0)
                    new_connect = randi(nearest_same_size);
                    G(child,near_notconn(nearest_same(new_connect))) = 1;
                    G(near_notconn(nearest_same(new_connect)),child) = 1;
                elseif (nearest_same_size == 0)
                    neigh_cut = neighbors(G_tmp,random_neighbor_cut);
                    near_cut = closest(random_neighbor_cut,:);
                    near_cut_already_conn = intersect(near_cut,neigh_cut);
                    near_cut_notconn = setdiff(near_cut,near_cut_already_conn);
                    nearest_cut_same = find(current_opinion(near_cut_notconn) == current_opinion(random_neighbor_cut));
                    nearest_cut_same_size = length(nearest_cut_same);
                    if (nearest_cut_same_size ~= 0)
                        new_conn = randi(nearest_cut_same_size);
                        G(random_neighbor_cut,near_cut_notconn(nearest_cut_same(new_conn))) = 1;
                        G(near_cut_notconn(nearest_cut_same(new_conn)),random_neighbor_cut) = 1;
                    elseif (nearest_cut_same_size == 0)
                        rand_person = randi(num_resp);
                        neigh_new = neighbors(G_tmp,rand_person);
                        near_new = closest(rand_person,:);
                        near_new_already_conn = intersect(near_new,neigh_new);
                        near_new_notconn = setdiff(near_new,near_new_already_conn);
                        nearest_new_same = find(current_opinion(near_new_notconn) == current_opinion(rand_person));
                        nearest_new_same_size = length(nearest_new_same);
                        if (nearest_new_same_size ~= 0)
                            new_rand_conn = randi(nearest_new_same_size);
                            G(rand_person,near_new_notconn(nearest_new_same(new_rand_conn))) = 1;
                            G(near_new_notconn(nearest_new_same(new_rand_conn)),rand_person) = 1;
                        end
                    end
                end
            end
        end
     end
     % Record metrics. 
     corr_ts(i) = corr(current_opinion,dat(:,3),'rows','complete');
     stay_ts(i) = sum(dat(:,2) == current_opinion)/1522;
     move_ts(i) = sum(dat(:,3) == current_opinion)/1522;
end        

A_end = G;
op_end = current_opinion;

% Visualize networks. 

% op0_end = find(current_opinion == 0);
% op1_end = find(current_opinion == 1);
% op2_end = find(current_opinion == 2);
% 
% figure()
% 
% G_end = graph(G);
% p_end = plot(G_end);
% p_end = plot(G_end,'NodeLabel',{});
% 
% highlight(p_end,op0_end,'NodeColor',[0.4660, 0.6740, 0.1880]);
% 
% highlight(p_end,op1_end,'NodeColor',[0, 0.4470, 0.7410]);
% 
% highlight(p_end,op2_end,'NodeColor',[0.75, 0, 0.75]);
% 
% set(gca,'XColor', 'none','YColor','none')
% p_end.MarkerSize = 12;
% p_end.EdgeColor = 'black';
% p_end.LineWidth = 1.5;

end
