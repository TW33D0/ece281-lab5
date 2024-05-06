--+----------------------------------------------------------------------------
--|
--| NAMING CONVENSIONS :
--|
--|    xb_<port name>           = off-chip bidirectional port ( _pads file )
--|    xi_<port name>           = off-chip input port         ( _pads file )
--|    xo_<port name>           = off-chip output port        ( _pads file )
--|    b_<port name>            = on-chip bidirectional port
--|    i_<port name>            = on-chip input port
--|    o_<port name>            = on-chip output port
--|    c_<signal name>          = combinatorial signal
--|    f_<signal name>          = synchronous signal
--|    ff_<signal name>         = pipeline stage (ff_, fff_, etc.)
--|    <signal name>_n          = active low signal
--|    w_<signal name>          = top level wiring signal
--|    g_<generic name>         = generic
--|    k_<constant name>        = constant
--|    v_<variable name>        = variable
--|    sm_<state machine type>  = state machine type definition
--|    s_<signal name>          = state name
--|
--+----------------------------------------------------------------------------
--|
--| ALU OPCODES:
--|
--|     ADD                     000
--|     SUB                     001
--|     OR                      010
--|     AND                     011
--|     Left Logical Shift      10X
--|     Right Logical Shift     11X
--+----------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;


entity ALU is
    port(
        -- Inputs
        i_A         : in std_logic_vector(7 downto 0);
        i_op        : in std_logic_vector(2 downto 0);
        i_B         : in std_logic_vector(7 downto 0);
        -- Outputs
        o_result    : out std_logic_vector(7 downto 0);
        o_flags     : out std_logic_vector(2 downto 0)
    );
end ALU;

architecture ALU_arch of ALU is 
  
-- Declare components and signals
    -- Declare Componenets
        -- declare left shifter component
    component leftshifter is
        port(
            i_BL     : in std_logic_vector(7 downto 0);
            i_AL     : in std_logic_vector(7 downto 0);
            o_shift  : out std_logic_vector(7 downto 0)
        );
    end component leftshifter; 
     
        -- declare right shifter component
    component rightshifter is
        port(
            i_BR    : in std_logic_vector(7 downto 0);
            i_AR    : in std_logic_vector(7 downto 0);
            o_shift : out std_logic_vector(7 downto 0)
        );
    end component rightshifter;     
    
        -- declare flags component
    component flags is
        port(
            i_result  : in std_logic_vector(7 downto 0);
            o_sign    : out std_logic;
            o_zero    : out std_logic
        );
    end component flags;
    
    -- Declare Signals
    signal w_ADD_SUM : std_logic_vector(7 downto 0);  -- ouput from ADD/SUB MUX
    signal w_Cout    : std_logic;                     -- connects Cout from the adder to o_flags
    signal w_adder   : std_logic_vector(7 downto 0);  -- connects ADD/SUB to MUX
    signal w_AND     : std_logic_vector(7 downto 0);  -- connects AND gate to mini MUX
    signal w_OR      : std_logic_vector(7 downto 0);  -- connects OR gate to mini MUX
    signal w_AND_OR  : std_logic_vector(7 downto 0);  -- connnects the AND/OR mini MUX to MUX
    signal w_Lshift  : std_logic_vector(7 downto 0);  -- connects leftshifter to MUX
    signal w_Rshift  : std_logic_vector(7 downto 0);  -- connects rightshifter to MUX
    signal w_result  : std_logic_vector(7 downto 0);  -- connects MUX to flags component
    signal w_nB      : std_logic_vector(7 downto 0);  -- makes it i_B not
    signal w_sub     : std_logic_vector(7 downto 0);  -- connects subtration to MUX

begin
	-- PORT MAPS ----------------------------------------
        -- port map for left shifter
    leftshifter_arch : leftshifter
        port map( 
            i_BL    => i_B,
            i_AL    => i_A,
            o_shift => w_Lshift
        );
        
        -- port map for right shifter    
    rightshifter_arch : rightshifter
        port map( 
            i_BR    => i_B,
            i_AR    => i_A,
            o_shift => w_Rshift
        );
    
    flags_arch : flags
        port map( 
            i_result    => w_result,
            o_sign      => o_flags(2),
            o_zero      => o_flags(1)
        );

    -- CONCURRENT STATEMENTS ----------------------------
	  
	  w_AND <= i_A and i_B;
	  w_OR <= i_A or i_B;
	  w_nB <= not i_B;
	  w_sub <= std_logic_vector(unsigned(w_nB) + 1);
	  
	   -- Adder MUX
	w_ADD_SUM <= i_B when i_op(0) = '0' else
	             w_sub;

	   -- AND/OR MUX
	w_AND_OR <= w_OR when i_op(0) = '0' else
	            w_AND;
   
        -- Adder MUX
    w_result <= w_adder when i_op(2 downto 1) = "00" else
                w_AND_OR when i_op(2 downto 1) = "01" else
                w_Lshift when i_op(2 downto 1) = "10" else
                w_Rshift;
  
    w_Cout <= (i_B(7) AND i_A(7)) or
              (i_B(6) AND i_A(6)) or
              (i_B(5) AND i_A(5)) or
              (i_B(4) AND i_A(4)) or
              (i_B(3) AND i_A(3)) or
              (i_B(2) AND i_A(2)) or
              (i_B(1) AND i_A(1)) or
              (i_B(0) AND i_A(0)) when i_op = "000" else
              (w_nB(7) AND i_A(7)) or
              (w_nB(6) AND i_A(6)) or
              (w_nB(5) AND i_A(5)) or
              (w_nB(4) AND i_A(4)) or
              (w_nB(3) AND i_A(3)) or
              (w_nB(2) AND i_A(2)) or
              (w_nB(1) AND i_A(1)) or
              (w_nB(0) AND i_A(0)) when i_op = "001" else
              '0';
    o_flags(0) <= w_Cout;
    w_adder <= std_logic_vector(unsigned(w_ADD_SUM) + unsigned(i_A));
    o_result <= w_result;
    
end ALU_arch;
