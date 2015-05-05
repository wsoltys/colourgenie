-- Colour Genie keyboard Matrix by wsoltys based on the code by Mike Stirling
--
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in synthesized form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name of the author nor the names of other contributors may
--   be used to endorse or promote products derived from this software without
--   specific prior written agreement from the author.
--
-- * License is granted for non-commercial use only.  A fee may not be charged
--   for redistributions as source code or in synthesized/hardware form without 
--   specific prior written agreement from the author.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--

-- PS/2 scancode to Colour Genie matrix conversion
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity keyboard is
port (
	CLK			:	in	std_logic;
	nRESET		:	in	std_logic;

	-- PS/2 interface
	PS2_CLK		:	in	std_logic;
	PS2_DATA	:	in	std_logic;
	
	-- CPU address bus (row)
	A			:	in	std_logic_vector(15 downto 0);
	-- Column outputs to ULA
	KEYB		:	out	std_logic_vector(7 downto 0);

	F11		:	out	std_logic
	);
end keyboard;

architecture rtl of keyboard is

-- PS/2 interface
component ps2_intf is
generic (filter_length : positive := 8);
port(
	CLK			:	in	std_logic;
	nRESET		:	in	std_logic;
	
	-- PS/2 interface (could be bi-dir)
	PS2_CLK		:	in	std_logic;
	PS2_DATA	:	in	std_logic;
	
	-- Byte-wide data interface - only valid for one clock
	-- so must be latched externally if required
	DATA		:	out	std_logic_vector(7 downto 0);
	VALID		:	out	std_logic;
	ERROR		:	out	std_logic
	);
end component;

-- Interface to PS/2 block
signal keyb_data	:	std_logic_vector(7 downto 0);
signal keyb_valid	:	std_logic;
signal keyb_error	:	std_logic;

-- Internal signals
type key_matrix is array (7 downto 0) of std_logic_vector(7 downto 0);
signal keys		:	key_matrix;
signal release	:	std_logic;
signal extended	:	std_logic;
begin	

	ps2 : ps2_intf port map (
		CLK, nRESET,
		PS2_CLK, PS2_DATA,
		keyb_data, keyb_valid, keyb_error
		);

	-- Output addressed row to ULA
	KEYB <= keys(0) when A(0) = '0' else
		keys(1) when A(1) = '0' else
		keys(2) when A(2) = '0' else
		keys(3) when A(3) = '0' else
		keys(4) when A(4) = '0' else
		keys(5) when A(5) = '0' else
		keys(6) when A(6) = '0' else
		keys(7) when A(7) = '0' else
		(others => '1');

	process(nRESET,CLK)
	begin
		if nRESET = '0' then
			release <= '0';
			extended <= '0';
			
			keys(0) <= (others => '1');
			keys(1) <= (others => '1');
			keys(2) <= (others => '1');
			keys(3) <= (others => '1');
			keys(4) <= (others => '1');
			keys(5) <= (others => '1');
			keys(6) <= (others => '1');
			keys(7) <= (others => '1');
		elsif rising_edge(CLK) then
			if keyb_valid = '1' then
				if keyb_data = X"e0" then
					-- Extended key code follows
					extended <= '1';
				elsif keyb_data = X"f0" then
					-- Release code follows
					release <= '1';
				else
					-- Cancel extended/release flags for next time
					release <= '0';
					extended <= '0';
				
					case keyb_data is
          -- keys(address line)(data line)
					--when X"12" => keys(0)(0) <= release; -- @
					when X"1c" => keys(0)(1) <= release; -- A
					when X"32" => keys(0)(2) <= release; -- B
					when X"21" => keys(0)(3) <= release; -- C
					when X"23" => keys(0)(4) <= release; -- D
					when X"24" => keys(0)(5) <= release; -- E
					when X"2b" => keys(0)(6) <= release; -- F
					when X"34" => keys(0)(7) <= release; -- G
					
					when X"33" => keys(1)(0) <= release; -- H
					when X"43" => keys(1)(1) <= release; -- I
					when X"3b" => keys(1)(2) <= release; -- J
					when X"42" => keys(1)(3) <= release; -- K
					when X"4b" => keys(1)(4) <= release; -- L
					when X"3a" => keys(1)(5) <= release; -- M
					when X"31" => keys(1)(6) <= release; -- N
					when X"44" => keys(1)(7) <= release; -- O
					
					when X"4d" => keys(2)(0) <= release; -- P
					when X"15" => keys(2)(1) <= release; -- Q
					when X"2d" => keys(2)(2) <= release; -- R
					when X"1b" => keys(2)(3) <= release; -- S
					when X"2c" => keys(2)(4) <= release; -- T
					when X"3c" => keys(2)(5) <= release; -- U
					when X"2a" => keys(2)(6) <= release; -- V
					when X"1d" => keys(2)(7) <= release; -- W
				
					when X"22" => keys(3)(0) <= release; -- X
					when X"35" => keys(3)(1) <= release; -- Y
					when X"1a" => keys(3)(2) <= release; -- Z
					--
					when X"05" => keys(3)(4) <= release; -- F1
					when X"06" => keys(3)(5) <= release; -- F2
					when X"04" => keys(3)(6) <= release; -- F3
					when X"0c" => keys(3)(7) <= release; -- F4
					
					when X"45" => keys(4)(0) <= release; -- 0
					when X"16" => keys(4)(1) <= release; -- 1
					when X"1e" => keys(4)(2) <= release; -- 2
					when X"26" => keys(4)(3) <= release; -- 3
					when X"25" => keys(4)(4) <= release; -- 4
					when X"2e" => keys(4)(5) <= release; -- 5
					when X"36" => keys(4)(6) <= release; -- 6
					when X"3d" => keys(4)(7) <= release; -- 7
					
					when X"3e" => keys(5)(0) <= release; -- 8
					when X"46" => keys(5)(1) <= release; -- 9
					--when X"" => keys(5)(2) <= release; -- : ?
					when X"4c" => keys(5)(3) <= release; -- ;
					when X"41" => keys(5)(4) <= release; -- ,
					when X"4e" => keys(5)(5) <= release; -- -
					when X"49" => keys(5)(6) <= release; -- .
					when X"4a" => keys(5)(7) <= release; -- /
					
					--when X"" => keys(6)(0) <= release; -- NL ?
					when X"03" => keys(6)(1) <= release; -- CLEAR -> F5
					when X"0b" => keys(6)(2) <= release; -- BREAK -> F6
					-- Cursor keys - these are actually extended (E0 xx), but
					-- the scancodes for the numeric keypad cursor keys are
					-- are the same but without the extension, so we'll accept
					-- the codes whether they are extended or not
					when X"75" => keys(6)(3) <= release; -- Up
					when X"72" => keys(6)(4) <= release; -- Down
					when X"6b" => keys(6)(5) <= release; -- Left
					when X"74" => keys(6)(6) <= release; -- Right
					when X"29" => keys(6)(7) <= release; -- SPACE
                    
					when X"12" => keys(7)(0) <= release; -- LSHIFT
					when X"59" => keys(7)(0) <= release; -- RSHIFT
					when X"83" => keys(7)(1) <= release; -- Mode Select -> F7
					when X"0a" => keys(7)(3) <= release; -- RPT -> F8
					when X"14" => keys(7)(4) <= release; -- CTRL
					--when X"" => keys(7)(7) <= release; -- LP
				
									
					-- example for key combinations
					--when X"66" =>	keys(0)(0) <= release; -- Backspace (CAPS 0)
					--				keys(4)(0) <= release;
					
					when X"78" =>	F11 <= release;        -- F11 key

					when others =>
						null;
					end case;
				end if;
			end if;
		end if;
	end process;

end architecture;
