library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;


entity CGENIE_TOP is
  port (
  
    RESET_n       : in std_logic;
    CLK17M        : in std_logic; -- system clock at 17.735 MHz
    
    PS2_CLK       : in std_logic;
    PS2_DATA      : in std_logic;
    
    NMI_n     : in std_logic
  
    );
end;


architecture RTL of CGENIE_TOP is

  -- clocks
  signal clock   : std_logic;
  signal clk_div : unsigned(3 downto 0);
  signal qaclk   : std_logic;
  signal strobesl: std_logic;

  -- CPU signals
  signal cpu_clken	:	std_logic;
  signal cpu_wait_n	:	std_logic;
  signal cpu_irq_n	:	std_logic;
  signal cpu_nmi_n	:	std_logic;
  signal cpu_busreq_n	:	std_logic;
  signal cpu_m1_n		:	std_logic;
  signal cpu_mreq_n	:	std_logic;
  signal cpu_ioreq_n	:	std_logic;
  signal cpu_rd_n		:	std_logic;
  signal cpu_wr_n		:	std_logic;
  signal cpu_rfsh_n	:	std_logic;
  signal cpu_halt_n	:	std_logic;
  signal cpu_busack_n	:	std_logic;
  signal cpu_a		:	std_logic_vector(15 downto 0);
  signal cpu_di		:	std_logic_vector(7 downto 0);
  signal cpu_do		:	std_logic_vector(7 downto 0);
  
  -- CRTC signals
  signal cclk         : std_logic;
  signal crtc_clken		:	std_logic; 
  signal crtc_enable  : std_logic;
  signal crtc_do			:	std_logic_vector(7 downto 0);
  signal crtc_vsync		:	std_logic;
  signal crtc_hsync		:	std_logic;
  signal crtc_de			:	std_logic;
  signal crtc_cursor	:	std_logic;
  signal crtc_lpstb		:	std_logic := '0';
  signal crtc_ma			:	std_logic_vector(13 downto 0);
  signal crtc_ra			:	std_logic_vector(4 downto 0);
  
  -- ROM signals
  signal rom_do     : std_logic_vector(7 downto 0);
  signal crom_do    : std_logic_vector(7 downto 0);
  signal crom_addr  : std_logic_vector(10 downto 0);
  signal crom_addra : std_logic_vector(2 downto 0);
  signal rom_enable : std_logic;
  signal crom_enable: std_logic;
  
  -- RAM signals
  signal ram1_enable : std_logic;
  signal ram1_wr_en  : std_logic;
  signal ram2_enable : std_logic;
  signal ram1_do     : std_logic_vector(7 downto 0);
  signal ram1_addr	 : std_logic_vector(13 downto 0);
  signal pcram_do    : std_logic_vector(7 downto 0);
  signal pcram_addr	 : std_logic_vector(9 downto 0);
  signal pcram_wr_en : std_logic;
  signal x2ram_do    : std_logic_vector(7 downto 0);
  signal pcg_enable  : std_logic;
  signal ccm_enable  : std_logic;
  signal cram_do     : std_logic_vector(3 downto 0);
  signal cram_do_r   : std_logic_vector(3 downto 0);
  signal cram_wr_en  : std_logic;
  
  signal char_do     : std_logic_vector(7 downto 0);
  signal keyb_enable : std_logic;
  signal keyb_do     : std_logic_vector(7 downto 0);
  signal sigFF_enable: std_logic;
  signal sigZ25      : std_logic_vector(5 downto 0);
  signal clkZ25      : std_logic;
  signal srout_n     : std_logic;







begin


  -- generate the different clocks from 17.735MHz main clock
  process(CLK17M)
  begin
    if rising_edge(CLK17M) then
      clk_div <= clk_div + 1;
    end if;
    
    cclk      <= clk_div(3); -- vid clock 1.1 MHz
    cpu_clken <= clk_div(2); -- main cpu clock 2.2 MHz
    qaclk     <= clk_div(0); -- 8.8 MHz
  end process;

  clock <= CLK17M;

  -- CPU
  cpu: entity work.T80se 
    port map (
      RESET_n   => RESET_n,
      CLK_n     => clock,
      CLKEN     => cpu_clken,
      WAIT_n    => cpu_wait_n,
      INT_n     => cpu_irq_n,
      NMI_n     => NMI_n, --cpu_nmi_n,
      BUSRQ_n   => cpu_busreq_n,
      M1_n      => cpu_m1_n,
      MREQ_n    => cpu_mreq_n,
      IORQ_n    => cpu_ioreq_n,
      RD_n      => cpu_rd_n,
      WR_n      => cpu_wr_n,
      RFSH_n    => cpu_rfsh_n,
      HALT_n    => cpu_halt_n,
      BUSAK_n   => cpu_busack_n,
      A         => cpu_a,
      DI        => cpu_di,
      DO        => cpu_do
    );
    
  -- cpu data bus mux
  cpu_di <= ram1_do when ram1_enable = '1' else
            rom_do when rom_enable = '1' else
            crtc_do when crtc_enable = '1' else
            keyb_do when keyb_enable = '1' else
            "0000" & cram_do when ccm_enable = '1' and cpu_rd_n = '0' else
            char_do when pcg_enable = '1' and cpu_rd_n = '0' else
            -- Idle bus
            (others => '1');

  -- Unused CPU input signals
  cpu_wait_n   <= '1';
  cpu_irq_n    <= '1';
  cpu_busreq_n <= '1';
  cpu_nmi_n    <= '1';
  -- trigger nmi either with F11, the OSD or with the joystick
  --cpu_nmi_n <= '0' when key_f11 = '1' or status(2) = '1' or joystickA(7) = '1' or joystickB(7) = '1' else '1';
  
  clkZ25 <= cpu_wr_n or not sigFF_enable;
  
  process(clkZ25)
  begin
    if RESET_n = '0' then
      sigZ25 <= (others=>'0');
    elsif rising_edge(clkZ25) then
      -- /CTRQ, C, B, PGS, SER, CASSWR
      sigZ25 <= cpu_do(5 downto 0);
    end if;
  end process;
  
  -- crtc
  crtc : entity work.mc6845 
    port map (
      CLOCK   => clock,
      CLKEN   => crtc_clken,
      nRESET  => RESET_n,
      ENABLE  => crtc_enable,
      R_nW    => cpu_wr_n,
      RS      => cpu_a(0),
      DI      => cpu_do,
      DO      => crtc_do,
      VSYNC   => crtc_vsync,
      HSYNC   => crtc_hsync,
      DE      => crtc_de,
      CURSOR  => crtc_cursor,
      LPSTB   => crtc_lpstb,
      MA      => crtc_ma,
      RA      => crtc_ra 
    );
    
  crtc_clken  <= cpu_clken;
  
  -- 16KB BASIC ROM
  rom: entity work.CG_BASIC
    port map 
    (
      CLK   => clock,
      ADDR  => cpu_a(13 downto 0),
      DATA  => rom_do
    );
    
  -- 16KB RAM
  ram1 : entity work.spram
    generic map
    (
      widthad_a	=> 14
    )
    port map
    (
      address   => ram1_addr,
      clock     => clock,
      data      => cpu_do,
      wren      => ram1_wr_en,
      q         => ram1_do
    );
    
  ram1_wr_en <= ram1_enable and not cpu_wr_n;
  ram1_addr <= cpu_a(13 downto 0) when cclk = '0' else crtc_ma;
  
  -- keyboard
  kb:	entity work.keyboard 
    port map (
      CLK       => clock,
      nRESET    => reset_n,
      PS2_CLK   => PS2_CLK,
      PS2_DATA  => PS2_DATA,
      A         => cpu_a,
      KEYB      => keyb_do
		);
    
  -- address decoding
  --
  -- ROM is enabled between 0x0000 and 0x3fff
  rom_enable <= (not cpu_mreq_n) and not (cpu_a(15) or cpu_a(14)); -- and not cpu_rfsh_n?
  -- RAM1 is enabled between 0x4000 and 0x7FFF
	ram1_enable <= not (cpu_mreq_n or cpu_a(15)) and cpu_a(14);
  -- RAM2 is enabled between 0x8000 and 0xBFFF
  ram2_enable <= not (cpu_mreq_n or cpu_a(14)) and cpu_a(15);
  -- enabled when IORQ = low and A7-A0 = "1111101X" (FA or FB)
  crtc_enable <= (not cpu_ioreq_n) and cpu_a(7) and cpu_a(6) and cpu_a(5) and cpu_a(4) and cpu_a(3) and not cpu_a(2) and cpu_a(1);
  -- A7-A0 = FF
  sigFF_enable <= '1' when cpu_a(7 downto 0)="11111111" else '0';
  -- keyboard is enabled between 0xF800 and 0xFBFF
  keyb_enable <= (not cpu_mreq_n) and cpu_a(15) and cpu_a(14) and cpu_a(13) and cpu_a(12) and cpu_a(11) and not cpu_a(10);
  -- programmable character generator programm output F400-F7FF
  pcg_enable <= (not cpu_mreq_n) and cpu_a(15) and cpu_a(14) and cpu_a(13) and cpu_a(12) and not cpu_a(11) and cpu_a(10);
  -- colour ram enabled F000-F3FF
  ccm_enable <= (not cpu_mreq_n) and cpu_a(15) and cpu_a(14) and cpu_a(13) and cpu_a(12) and not cpu_a(11) and not cpu_a(10);
  
  -- video
  
  -- 2k character rom
  char_rom: entity work.CG_FONT
    port map 
    (
      CLK   => clock,
      ADDR  => crom_addr,
      DATA  => crom_do
    );
  
  crom_addr <= x2ram_do & crom_addra;
  crom_addra <= cpu_a(2 downto 0) when pcg_enable = '1' else crtc_ra(2 downto 0);
 
  -- 1k prog char RAM
  char_ram : entity work.spram
    generic map
    (
      widthad_a	=> 10
    )
    port map
    (
      address   => pcram_addr,
      clock     => clock,
      data      => cpu_do,
      wren      => pcram_wr_en,
      q         => pcram_do
    );
  
  -- -> not(C and not D6) and not(B and D6) and D7
  strobesl <= not(not(sigZ25(4) and not x2ram_do(6)) and not(sigZ25(3) and x2ram_do(6)) and x2ram_do(7));
  crom_enable <= '0' when pcg_enable = '1' else strobesl;
  char_do <= pcram_do when crom_enable = '0' else crom_do;
    
  pcram_wr_en <= (not cpu_wr_n) and pcg_enable;
  
  -- signal /SROUT
  process(qaclk)
    variable shift_reg : std_logic_vector(7 downto 0);
  begin
    if sigZ25(5) = '0' then
      shift_reg := (others=>'0');
    elsif rising_edge(qaclk) then
      if strobesl = '0' then
        shift_reg := char_do;
      else
        shift_reg := shift_reg(6 downto 0) & '0';
      end if;
      srout_n <= shift_reg(7);
    end if;
  end process;
    
  -- Colour RAM
  colour_ram : entity work.spram
    generic map
    (
      widthad_a	=> 10,
      width_a   => 4
    )
    port map
    (
      address   => ram1_addr(9 downto 0),
      clock     => clock,
      data      => cpu_do(3 downto 0),
      wren      => cram_wr_en,
      q         => cram_do
    ); 
    
  cram_wr_en <= (not cpu_wr_n) and ccm_enable;
  
	process(cclk)
	begin
		-- latch ram data at the end of CRTC read cycle (Z8)
		if falling_edge(cclk) and pcg_enable='0' then
			x2ram_do <= ram1_do;
		end if;
	end process;
  
  process(cclk)
	begin
		-- delay colour data by one clock cycle (Z18)
    -- cram_do_r -> cc0 to cc3
		if falling_edge(cclk) then
			cram_do_r <= cram_do;
		end if;
	end process;

end RTL;