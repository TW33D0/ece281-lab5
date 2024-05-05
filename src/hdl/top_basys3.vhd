--+----------------------------------------------------------------------------
--|
--| FILENAME      : top_basys3.vhd
--| AUTHOR(S)     : C3C Ty Hubert
--| CREATED       : 04/30/2024 Last Modified: 05/01/2024
--| DESCRIPTION   : This file implements top level code for a CPU.
--|  
--| DOCUMENTATION : None
--|
--+----------------------------------------------------------------------------
--|
--| REQUIRED FILES :
--|
--|    Libraries : ieee
--|    Packages  : std_logic_1164, numeric_std, unisim
--|    Files     : None
--|
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
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;


entity top_basys3 is
    port(
        -- inputs
        clk     :   in std_logic; -- native 100MHz FPGA clock
        sw      :   in std_logic_vector(15 downto 0);
        btnU    :   in std_logic; -- master_reset
        btnC    :   in std_logic; -- next stage input
        
        -- outputs
        led :   out std_logic_vector(15 downto 0);
        -- 7-segment display segments (active-low cathodes)
        seg :   out std_logic_vector(6 downto 0);
        -- 7-segment display active-low enables (anodes)
        an  :   out std_logic_vector(3 downto 0)
    );
end top_basys3;

architecture top_basys3_arch of top_basys3 is 
  
-- Declare Components and Signals
    
    -- Declare Components
        -- declare controller component
component controller_fsm is
    port( 
        i_clk     : in  std_logic;
        i_reset   : in  std_logic;
        i_adv     : in  std_logic;
        o_cycle   : out std_logic_vector(3 downto 0)
    );
end component controller_fsm;

        -- declare clock divider component
component clock_divider is
    generic ( constant k_DIV : natural := 2);
    port (     
        i_clk    : in std_logic;           -- Basys3 clock
        o_clk    : out std_logic           -- divided (slow) clock
    );
end component clock_divider;
        
        -- declare regulator A component
component regA is
    port (
        i_clk   : in std_logic_vector(3 downto 0);
        i_A     : in std_logic_vector(7 downto 0);
        o_A     : out std_logic_vector(7 downto 0)
    );
end component regA;

        -- declare regulator B component
component regB is
    port (
        i_clk   : in std_logic_vector(3 downto 0);
        i_B     : in std_logic_vector(7 downto 0);
        o_B     : out std_logic_vector(7 downto 0)
    );
end component regB;

        -- declare ALU component
component ALU is
    port (
        i_A         : in std_logic_vector(7 downto 0);
        i_op        : in std_logic_vector(2 downto 0);
        i_B         : in std_logic_vector(7 downto 0);
        o_result    : out std_logic_vector(7 downto 0);
        o_flags     : out std_logic_vector(2 downto 0)
    );
end component ALU;

        -- declare Answer to Unit conversion component
component twoscomp_decimal is
    port (
        i_bin   : in std_logic_vector(7 downto 0);
        o_sign  : out std_logic_vector(3 downto 0);
        o_hund  : out std_logic_vector(3 downto 0);
        o_tens  : out std_logic_vector(3 downto 0);
        o_ones  : out std_logic_vector(3 downto 0)
    );
end component twoscomp_decimal;

        -- declare TDM component
component TDM4 is
    port (
        i_clk   : in std_logic;
        i_D0    : in std_logic_vector(3 downto 0);
        i_D1    : in std_logic_vector(3 downto 0);
        i_D2    : in std_logic_vector(3 downto 0);
        i_D3    : in std_logic_vector(3 downto 0);
        o_data  : out std_logic_vector(3 downto 0);
        o_sel   : out std_logic_vector(3 downto 0)
    );
end component TDM4;

        -- declare 7 Segment Decoder component
component sevenSegDecoder is
    port (
        i_D     : in std_logic_vector(3 downto 0);
        o_S     : out std_logic_vector(6 downto 0)
    );
end component sevenSegDecoder;

    -- Declare Signals
signal w_cycle  : std_logic_vector(3 downto 0);   -- output from state controller
signal w_A      : std_logic_vector(7 downto 0);   -- connects regA to ALU and MUX
signal w_B      : std_logic_vector(7 downto 0);   -- connects regB to ALU and MUX
signal w_result : std_logic_vector(7 downto 0);   -- connects ALU output to the MUX
signal w_bin    : std_logic_vector(7 downto 0);   -- connects MUX number convertor
signal w_sign   : std_logic_vector(3 downto 0);   -- connects the converter sign to the TDM
signal w_hund   : std_logic_vector(3 downto 0);   -- connects the hundreds value to the TDM
signal w_tens   : std_logic_vector(3 downto 0);   -- connects the tens value to the TDM
signal w_ones   : std_logic_vector(3 downto 0);   -- connects the ones value to the TDM
signal w_clk    : std_logic;                      -- connects the slowed clock to the TDM
signal w_sel    : std_logic_vector(3 downto 0);   -- connects the SEL to the anode MUX
signal w_data   : std_logic_vector(3 downto 0);   -- connects TDM to 7SegDecoder
signal w_flags  : std_logic_vector(2 downto 0);   -- connects flags to LEDs

begin
	-- PORT MAPS ----------------------------------------
        -- port map for controller
	controller_fsm_arch : controller_fsm
    port map( 
        i_clk   => clk,
        i_reset => btnU,
        i_adv   => btnC,
        o_cycle => w_cycle
    );
    
        -- port map for clock divider
    clkdiv_inst : clock_divider    -- clock = 100 MHz / (2*k_DIV)
    generic map(k_DIV => 200000)   -- k_DIV = 50 MHz / clock
    port map(                      -- 250 Hz clock from 100 MHz
        i_clk => clk,
        o_clk => w_clk
    );
        
        -- port map for Regulator A
    regA_arch : regA
    port map(
        i_clk => w_cycle,
        i_A   => sw(7 downto 0),
        o_A   => w_A
    );
    
        -- port map for Regulator B
    regB_arch : regB
    port map(
        i_clk => w_cycle,
        i_B   => sw(7 downto 0),
        o_B   => w_B
    );
    
        -- port map for ALU
    ALU_arch : ALU
    port map(
        i_A      => w_A,
        i_op     => sw(2 downto 0),
        i_B      => w_B,
        o_result => w_result,
        o_flags  => w_flags
    );

    
        -- port map for Answer to Unit conversion
    Behavioral : twoscomp_decimal
    port map(
        i_bin  => w_bin,
        o_sign => w_sign,
        o_hund => w_hund,
        o_tens => w_tens,
        o_ones => w_ones
    );

        -- port map for TDM
    TDM4_arch : TDM4
    port map(
        i_clk  => w_clk,
        i_D3   => w_sign,
        i_D2   => w_hund,
        i_D1   => w_tens,
        i_D0   => w_ones,
        o_data => w_data,
        o_sel  => w_sel
    );
    
        -- port map for 7SegDecoder
    sevenSegDecoder_arch : sevenSegDecoder
    port map(
        i_D => w_data,
        o_S => seg(6 downto 0)
    );
	
	-- CONCURRENT STATEMENTS ----------------------------
	
	   -- MUX 1
	w_bin <= w_A when (w_cycle = "0010" or w_cycle = "0000" or w_cycle = "0100") else
	         w_B when (w_cycle = "0111" or w_cycle = "0110") else
	         w_result when w_cycle = "1000" else
	         "00000000";
	       
	   -- MUX 2
	an <= (others => '1') when w_cycle = "0001" else 
	      w_sel;
	
	led(3 downto 0) <= w_cycle;
    led(15 downto 13) <= w_flags when w_cycle = "1000" else
                         "000";
	
end top_basys3_arch;
