

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity mist_cg is

  port (
    -- Clocks
    
    CLOCK_27    : in std_logic_vector(1 downto 0); -- 27 MHz


    -- SDRAM
    SDRAM_nCS : out std_logic; -- Chip Select
    SDRAM_DQ : inout std_logic_vector(15 downto 0); -- SDRAM Data bus 16 Bits
    SDRAM_A : out std_logic_vector(12 downto 0); -- SDRAM Address bus 13 Bits
    SDRAM_DQMH : out std_logic; -- SDRAM High Data Mask
    SDRAM_DQML : out std_logic; -- SDRAM Low-byte Data Mask
    SDRAM_nWE : out std_logic; -- SDRAM Write Enable
    SDRAM_nCAS : out std_logic; -- SDRAM Column Address Strobe
    SDRAM_nRAS : out std_logic; -- SDRAM Row Address Strobe
    SDRAM_BA : out std_logic_vector(1 downto 0); -- SDRAM Bank Address
    SDRAM_CLK : out std_logic; -- SDRAM Clock
    SDRAM_CKE: out std_logic; -- SDRAM Clock Enable
    
    -- SPI
    SPI_SCK : in std_logic;
    SPI_DI : in std_logic;
    SPI_DO : out std_logic;
    SPI_SS2 : in std_logic;
    SPI_SS3 : in std_logic;
    CONF_DATA0 : in std_logic;

    -- VGA output
    

    VGA_HS,                                             -- H_SYNC
    VGA_VS : out std_logic;                             -- V_SYNC
    VGA_R,                                              -- Red[5:0]
    VGA_G,                                              -- Green[5:0]
    VGA_B : out std_logic_vector(5 downto 0);           -- Blue[5:0]
    
    -- Audio
    AUDIO_L,
    AUDIO_R : out std_logic;
    
    -- LEDG
    LED : out std_logic

    );
  
end mist_cg;

architecture rtl of mist_cg is

  constant CONF_STR : string := "CGENIE;;T1,Trigger NMI";

  function to_slv(s: string) return std_logic_vector is 
    constant ss: string(1 to s'length) := s; 
    variable rval: std_logic_vector(1 to 8 * s'length); 
    variable p: integer; 
    variable c: integer; 
  
  begin 
    for i in ss'range loop
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8)); 
    end loop; 
    return rval; 

  end function; 



  component user_io
    generic ( STRLEN : integer := 0 );
  
    port ( SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
           SPI_MISO : out std_logic;
           conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
           joystick_0 : out std_logic_vector(5 downto 0);
           joystick_1 : out std_logic_vector(5 downto 0);
           joystick_analog_0 : out std_logic_vector(15 downto 0);
           joystick_analog_1 : out std_logic_vector(15 downto 0);
           scandoubler_disable : out std_logic;
           status:    out std_logic_vector(7 downto 0);
           SWITCHES : out std_logic_vector(1 downto 0);
           BUTTONS : out std_logic_vector(1 downto 0);
           sd_sdhc : in std_logic;
           ps2_clk : in std_logic;
           ps2_kbd_clk : out std_logic;
           ps2_kbd_data : out std_logic
         );

  end component user_io;
  
  component data_io is
    port(sck: in std_logic;
         ss: in std_logic;
         sdi: in std_logic;
         downloading: out std_logic;
         size: out std_logic_vector(15 downto 0);
         clk: in std_logic;
         we: in std_logic;
         a: in std_logic_vector(12 downto 0);
         din: in std_logic_vector(7 downto 0);
         dout: out std_logic_vector(7 downto 0));
  end component;
  
  component osd
    port ( pclk, sck, ss, sdi, hs_in, vs_in, scanline_ena_h : in std_logic;
           red_in, blue_in, green_in : in std_logic_vector(5 downto 0);
           red_out, blue_out, green_out : out std_logic_vector(5 downto 0);
           hs_out, vs_out : out std_logic
         );
  end component osd;

  signal clk17m, osd_pclk, clk12k  : std_logic;
  signal force_reset : std_logic := '0';
  signal reset_n   : std_logic;
  
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy        : std_logic_vector(5 downto 0);
  signal joy0       : std_logic_vector(5 downto 0);
  signal joy1       : std_logic_vector(5 downto 0);
  signal joy_an0    : std_logic_vector(15 downto 0);
  signal joy_an1    : std_logic_vector(15 downto 0);
  signal joy_an     : std_logic_vector(15 downto 0);
  signal status     : std_logic_vector(7 downto 0);
  signal scandoubler_disable : std_logic;
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  
  signal pll_locked : std_logic;
  
  signal VGA_R_O  : std_logic_vector(5 downto 0);
  signal VGA_G_O  : std_logic_vector(5 downto 0);
  signal VGA_B_O  : std_logic_vector(5 downto 0);
  signal VGA_HS_O : std_logic;
  signal VGA_VS_O : std_logic;
  
  signal hsync_out : std_logic;
  signal vsync_out : std_logic;
  signal csync_out : std_logic;
  
  signal downl          : std_logic := '0';
  signal size           : std_logic_vector(15 downto 0) := (others=>'0');
  signal cart_a         : std_logic_vector(12 downto 0);
  signal cart_d         : std_logic_vector(7 downto 0);
  
  
  

begin
  
  reset_n <= not(status(0) or buttons(1) or not pll_locked);
 
 
  pllInstance : entity work.mist_pll
    port map (
      inclk0 => CLOCK_27(0),
      c0 => clk17m,
      c1 => clk12k,
      locked => pll_locked
    );
    
  
  cg_u : entity work.CGENIE_TOP
    port map (
      RESET_n   => reset_n,
      CLK17M    => clk17m,
      PS2_CLK   => ps2Clk,
      PS2_DATA  => ps2Data,
      
      NMI_n     => '1'--not status(1)
    );

  

  
  --osd_pclk <= clk_14 when scandoubler_disable='0' else ena;
  --
  -- a minimig vga->scart cable expects a composite sync signal on the VGA_HS output 
  -- and VCC on VGA_VS (to switch into rgb mode)
  --csync_out <= '1' when (hsync_out = vsync_out) else '0';
  --VGA_HS <= hsync_out when scandoubler_disable='0' else csync_out;
  --VGA_VS <= vsync_out when scandoubler_disable='0' else '1';
  
-----------------------------------------------------------------------------
-- MiST interfaces
  
  user_io_d : user_io
    generic map (STRLEN => CONF_STR'length)
    
    port map ( 
      SPI_CLK => SPI_SCK,
      SPI_SS_IO => CONF_DATA0,    
      SPI_MISO => SPI_DO,    
      SPI_MOSI => SPI_DI,       
      conf_str => to_slv(CONF_STR),
      status => status,   
      joystick_0 => joy0,   
      joystick_1 => joy1,
      joystick_analog_0 => joy_an0,
      joystick_analog_1 => joy_an1,
      scandoubler_disable => scandoubler_disable,
      SWITCHES => switches,   
      BUTTONS => buttons,
      sd_sdhc => '1',
      ps2_clk => clk12k,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );
    
  osd_inst : osd
    port map (
      pclk => osd_pclk,
      sdi => SPI_DI,
      sck => SPI_SCK,
      ss => SPI_SS3,
      red_in => VGA_R_O,
      green_in => VGA_G_O,
      blue_in => VGA_B_O,
      hs_in => VGA_HS_O,
      vs_in => VGA_VS_O,
      scanline_ena_h => '0',
      red_out => VGA_R,
      green_out => VGA_G,
      blue_out => VGA_B,
      hs_out => hsync_out,
      vs_out => vsync_out
    );
    
  --data_io_inst: data_io
    --port map(SPI_SCK, SPI_SS2, SPI_DI, downl, size, clk_14, '0', cart_a, (others=>'0'), cart_d);
    
    
  SDRAM_nCAS  <= '1'; -- disable sdram

end rtl;