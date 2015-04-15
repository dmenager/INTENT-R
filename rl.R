

act <- function(action, state){
	action.value <- action.values[[action]]#actin.values[] needs fixed
	new.state <- state

	#needs action movement

	if(is.na(rewards[new.state]))#rewards[] needs fixed
		new.state <- state

	return(new.state)
}

bellman.update <- function(action, state, values, gamma=1) {
	state.transition.prob <- transition[[action]]#transitions[] need fixed
	q <- rep(0, length(state.tansition.prob))
	for(i in 1:length(state.transition.prob)) {
		new.state <- act(names(state.transition.prob)[i], state)
		q[i] <- (state.transition.prob[i] * (rewards[state] + (gamma * values[new.state]))) #rewards[] and values[] need fixed
	}
	sum(q)
}

value.iteration <- function(states, actions, rewards, values, gamma, niter) {
	for(j in 1:niter) {
		for(i in 1:nrow(states)) {
			state <- unlist(states[i,])
			if(i %in% c(4,8)) next #fix for terminal state
			q.values <- as.numeric(lapply(actions, bellman.update, state=state, values=values, gamma=gamma))
			values[state] <- max(q.values)#values[] needs fixed
		}
	}
	return(values)
}

#function call 
#final.values <- value.iteration(states=states, action=actions, rewards=rewards, values=values, gamma=0.99, niter=100)