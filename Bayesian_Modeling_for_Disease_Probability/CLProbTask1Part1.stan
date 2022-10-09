
      generated quantities{
      real ntimes_I_got_infected = 0.0;
      real infected = 0.0;
      real n_persons;
      int person_I_meet_is_infected;
      real person_I_meet_infects_me;
      int i = 1;
      
      n_persons  = uniform_rng(25,30);
      
      while (i <= n_persons){
        person_I_meet_is_infected = bernoulli_rng(0.0001);
        person_I_meet_infects_me = uniform_rng(0.4,0.6);
        ntimes_I_got_infected += person_I_meet_is_infected * person_I_meet_infects_me;
        i = i + 1;
      }
      
      infected = ntimes_I_got_infected > 0;
      
      }
