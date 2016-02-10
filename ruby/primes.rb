class Primes
  def self.mod_exp(base, exp, mod)
    # Should probably raise an error for mod == 0...
    0 if mod == 1 or mod == 0
    # base must have enough precision for (mod -1)*(mod-1)
    res = 1
    base = base % mod
    while exp > 0
      if exp % 2 == 1
        res = (res*base) % mod
      end
      exp = exp /= 2
      base = (base*base) % mod
    end
    res
  end

  # witness also takes u and t to avoid recomputing the same values over and over
  # could be a member variable we if we made this into a class
  def self.witness(a, n, u, t)
    x = self.mod_exp(a, u, n)
    x_prev = x
    t.times do
      x = self.mod_exp(x_prev, 2, n)
      return true if x == 1 and x_prev != 1 and x_prev != n-1
      x_prev = x
    end
    return true if x != 1
    false
  end

  # tells if a given integer n is prime
  def self.mill_rab(n, k=18) # avg is 18-20 iterations

    # Short circuit on trivially composite numbers
    return false if n < 2 or (n != 2 and n % 2 == 0)

    # collect t and u as we express n-1 as 2^t * u
    # we also might be able to simply say u = 1 for all odd n
    # If we made a factorizing, or a mill_rab class, we could have these as member variables
    u = n-1
    t = 0
    while u % 2 == 0
      u /= 2
      t += 1
    end


    # Witness loop
    k.times do
      a = (2 + rand(n-2)) #rand(2..n-2)
      return false if self.witness(a, n, u, t)
    end
    true # almost surely
  end

  def self.naive_is_prime(n)
    # uses primes of form of 6k+/-1 to check
  end

  def self.trial_div(n)
    n_orig = n
    factors = []
    d = 2
    while d < Math.sqrt(n_orig).ceil+1
      while n % d == 0
        n /= d
        factors << d
      end
      d += 1
    end
    if n > 2
      factors << n
    end
    factors
  end

  def self.factorize(n)
    # First, use pollard's rho algorithm to collect the greatest factors of n
    # Some may be prime, so in the second phase of the algorithm, we decompose
    # each non-prime using trial-division.
    factors = []
    loop do
      d, n = self.poll_rho(n)
      if d != false
        factors << d
      else
        #factors << Raindrops.trial_div(n)
        break
      end
    end
    factors << n
    factors_prime = []
    factors.each { |f|
      if not self.mill_rab(f) # if f is prime
        factors_prime << self.trial_div(f)
      else
        factors_prime << f
      end
    }
    factors_prime.flatten!
    factors_prime.group_by { |x| x }
  end

  def self.poll_rho(n)
    # Pollard's rho algorithm
    # Probabilistically determine greatest factors of a given number

    i = 0

    x = rand(0..n-1) # uncomment later for efficiency, may have to redo
    y = x
    d = 1
    while i <= (n**(1/4.0))
      i += 1
      x = ((x**2) + 1) % n
      y = (((y**2 + 1)**2) + 1) % n
      d = n.gcd( (x-y).abs )
      if d != 1 and d != n
        return d, n/d
      end
    end
    return false, n
  end
end
