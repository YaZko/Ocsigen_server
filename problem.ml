

module type Problem =
    sig 
      type input
      type solution
      val parse_input : string -> input
      val parse_output : input -> string -> solution
      val score : input -> solution -> int
    end
      
      
