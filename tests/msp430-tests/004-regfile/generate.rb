def generate_zero_extend
  ids = []
  
  [1, 4, 16].each do |from|
    [16, 32, 64].select { _1 >= from }.each do |to|
      ids << id = "zero_extend_#{from}_#{to}"
      
      puts <<~END
        val #{id} : bits(#{from}) -> bits(#{to})
        function #{id}(v) = sail_zero_extend(v, #{to})

      END
      
      id
    end
  end

  puts "overload zero_extend = {#{ids.join(',')}}"
end

def generate_sign_extend
  ids = []
  
  [1, 16].each do |from|
    [16].select { _1 >= from }.each do |to|
      ids << id = "sign_extend_#{from}_#{to}"
      
      puts <<~END
        val #{id} : bits(#{from}) -> bits(#{to})
        function #{id}(v) = sail_sign_extend(v, #{to})

      END
      
      id
    end
  end

  puts "overload sign_extend = {#{ids.join(',')}}"
end

def generate_neg_vec
  ids = []
  
  [16, 32, 64].each do |nbits|
    ids << id = "neg_vec_#{nbits}"
    
    puts <<~END
      val #{id} : bits(#{nbits}) -> bits(#{nbits})
      function neg_vec_#{nbits}(v) = {
        let r = 0b0 in
        sub_bits(zero_extend(r), v)
      }

    END

    id
  end

  puts "overload neg = {#{ids.join(',')}}"
end


generate_zero_extend
generate_sign_extend
generate_neg_vec
