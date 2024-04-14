-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2023 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Nestor Baraniuk xbaran21@stud.fit.vutbr.cz
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
	signal PC 	: std_logic_vector(12 downto 0) := (others => '0');
	signal PC_inc   : std_logic;
	signal PC_dec   : std_logic;
	signal PTR 	: std_logic_vector(12 downto 0) := (others => '0');
	signal PTR_inc  : std_logic;
	signal PTR_dec  : std_logic;
	signal CNT 	: std_logic_vector(7 downto 0) := (others => '0');
	signal CNT_inc  : std_logic;
	signal CNT_dec  : std_logic;
	signal CNT_1    : std_logic;
	--signal CNT_0    : std_logic;
-- Define multiplexers
	signal MX1	: std_logic; -- Selects between program address (0) and data address (1)
	signal MX2	: std_logic_vector(1 downto 0); -- Selects the value to be written to memory: 00 - IN_DATA, 01 - (PTR-1), 10 - (PTR+1)

type STATES is (Idle, Init, Halt, Fetch, Decode, Increment, Decrement, Print, Scan_loop, Scan,
While_start, While_skip, While_start_exit, While_end, While_end_skip, While_end_loop, While_end_exit, Break, Break_loop);
signal PSTATE, NSTATE: STATES := Idle;

begin

pc_process : process (CLK, RESET)
  begin
	if RESET = '1' then
		PC <= (others => '0');
	elsif rising_edge(CLK) then
		if PC_inc = '1' then
			PC <= PC + 1;
		elsif PC_dec = '1' then
			PC <= PC - 1;
		end if;
	end if;
  end process;


ptr_process : process (CLK, RESET)
  begin
    if (RESET = '1') then
	    PTR <= (others => '0');
    elsif (rising_edge(CLK)) then
      if (PTR_inc = '1') then
      	PTR <= PTR + 1;
      elsif (PTR_dec = '1') then
	PTR <= PTR - 1;
      end if;
    end if;
  end process;

cnt_process : process (CLK, RESET)
  begin
    if (RESET = '1') then
            CNT <= (others => '0');
    elsif (rising_edge(CLK)) then
      if (CNT_inc = '1') then
        CNT <= CNT + 1;
      elsif (CNT_dec = '1') then
        CNT <= CNT - 1;
      elsif CNT_1 = '1' then
	CNT <= X"01";
      end if;
    end if;
  end process;
mx1_process : process (PC, PTR, MX1)
begin
  if MX1 = '0' then
      -- Code segment
      DATA_ADDR <= PC;
    else
      -- Data segment
      DATA_ADDR <= PTR;
  end if;
end process;

mx2_process : process (IN_DATA, DATA_RDATA, MX2)
     begin
          case MX2 is
               when "00"   => DATA_WDATA <= IN_DATA;
               when "01"   => DATA_WDATA <= DATA_RDATA - 1;
               when "10"   => DATA_WDATA <= DATA_RDATA + 1;
               when others => DATA_WDATA <= X"40";
          end case;
     end process;

FSM_P : process(CLK, RESET)
begin
  if RESET = '1' then
    -- Reset actions here
    PSTATE <= Idle;
  elsif rising_edge(CLK) then
    -- Regular clocked process goes here
    PSTATE <= NSTATE;
end if;
end process;

FSM_N : process(PSTATE, IN_VLD, OUT_BUSY, DATA_RDATA, EN, RESET)
begin
	--Set default values
          DATA_EN   <= '0';	-- when 1, data operations enabled
          DATA_RDWR <= '0';	-- read = 0, write = 1
          IN_REQ    <= '0';	-- input request
          OUT_WE    <= '0';
          OUT_DATA  <= X"00";
          MX1       <= '0';	-- 1 data sector, 0 code sector
          MX2	    <= "00";	-- 00 WDATA = IN_DATA, 01 WDATA = DATA_RDATA -1, 10 WDATA = DATA_RDATA +1
	  PC_inc    <= '0';	-- PC = PC + 1
	  PC_dec    <= '0';	-- PC = PC - 1
	  PTR_inc   <= '0';	-- PTR = PTR + 1
	  PTR_dec   <= '0';	-- PTR = PTR - 1
	  CNT_inc   <= '0';	-- CNT = CNT + 1
	  CNT_dec   <= '0';	-- CNT = CNT - 1
	  DONE      <= '0';	
	  CNT_1     <= '0';	-- CNT = 1
    case PSTATE is
	--IDLE
      when Idle =>
	READY <= '0';
        if EN = '1' then	--wait until EN = 1, then get ready to read
		MX1 <= '1';
		DATA_EN <= '1';
		NSTATE <= Init;
        else
          NSTATE <= Idle;
        end if;
	--Init, loop until getting X"40"
      when Init =>
		case DATA_RDATA is
                when X"40" => NSTATE <= Fetch;	--if X"40" => Start
			READY <= '1';		--READY <- 1, when mem[x] = X"40"
		when others => NSTATE <= Init;	--if not => get ready to read the next value
		MX1 <= '1';
		DATA_EN <= '1';
		PTR_inc <= '1';
		end case;
	--Start
	when Fetch => NSTATE <= Decode;		--Enable reading, read data is set by default
		DATA_EN <= '1';

	when Decode =>
		case DATA_RDATA is
		when X"40" => NSTATE <= Halt;	--when '@' -> Halt
		when X"3E" => NSTATE <= Fetch;	--when '>' -> PTR <- PTR + 1; PC <- PC + 1
			PC_inc <= '1';
			PTR_inc <= '1';
		when X"3C" => NSTATE <= Fetch;  --when '<' -> PTR <- PTR - 1; PC <- PC + 1
			PC_inc <= '1';
			PTR_dec <= '1';
		when X"2B" => NSTATE <= Increment; --when '+' -> PC <- PC + 1, enable work with data, DATA_ADDR is updated by default
			PC_inc <= '1';
			MX1 <= '1';
                        DATA_EN <= '1';
		when X"2D" => NSTATE <= Decrement;  --when '-' -> PC <- PC - 1, enable work with data, DATA_ADDR is updated by default
			PC_inc <= '1';
                        MX1 <= '1';
                        DATA_EN <= '1';
		when X"2E" => NSTATE <= Print;  --when '.' -> PC <- PC + 1, enable work with data, DATA_ADDR is updated by default
			PC_inc <= '1';
			MX1 <= '1';
			DATA_EN <= '1';
		when X"2C" => NSTATE <= Scan_loop;
		when X"5B" => NSTATE <= While_start; -- when '[' -> PC <- PC + 1, enable work with data, DATA_ADDR is updated by default
			MX1 <= '1';
			PC_inc <= '1';
			DATA_EN <= '1';
		when X"5D" => NSTATE <= While_end;   -- when ']' -> enable work with data, DATA_ADDR is updated by default
			MX1 <= '1';
			DATA_EN <= '1';
		when X"7E" => NSTATE <= Break;      -- when '~' -> PC <- PC + 1 
			PC_inc <= '1';
		when others => NSTATE <= Fetch;     -- move on to the next instruction, PC <- PC + 1
			PC_inc <= '1';
		end case;
		
	when Halt => NSTATE <= Halt;	--infinite loop to stop
                DONE <= '1';

	when Increment => NSTATE <= Fetch; --Working with data, WDATA = RDATA + 1
		MX2 <= "10";
		MX1 <= '1';
		DATA_RDWR <= '1';
		DATA_EN   <= '1';

	when Decrement => NSTATE <= Fetch; --Working with data, WDATA = RDATA - 1
                MX2 <= "01";
                MX1 <= '1';
                DATA_RDWR <= '1';
                DATA_EN   <= '1';

	when Print => NSTATE <= Fetch;	   
		if(OUT_BUSY = '1') then  --While (OUT_BUSY){}
			NSTATE <= Print;
		else  NSTATE <= Fetch;   --OUT_DATA <- RDATA
			OUT_WE	<= '1';
			OUT_DATA <= DATA_RDATA;
		end if;

	when Scan_loop =>		--While (!IN_VLD) { IN_REQ <- 1 }
		IN_REQ <= '1';
		if (IN_VLD = '0') then
			NSTATE <= Scan_loop;
		else    NSTATE <= Scan;
		end if;

	when Scan => NSTATE <= Fetch;  --PC <- PC + 1, DATA_WDATA <- IN_DATA by default, enable Write data
		MX1 <= '1';
		DATA_RDWR <= '1';
		DATA_EN <= '1';
		PC_inc <= '1';

	when While_start =>		
		if DATA_RDATA = X"00" then	-- if (mem[PTR] == 0)
			CNT_1 <= '1';		-- CNT <- 1
			DATA_EN <= '1';
			NSTATE <= While_skip;
		else
			NSTATE <= Fetch;
		end if;
	when While_skip => NSTATE <= While_start_exit;  
		if CNT = X"00" then NSTATE <= Fetch;	--While (CNT != 0)
                else NSTATE <= While_start_exit;
                        DATA_EN <= '1';			-- c <- mem[PC]
                end if;
	when While_start_exit => NSTATE <= While_skip;
		if DATA_RDATA = X"5B" then		--if (c == '[') CNT <- CNT + 1
			CNT_inc <= '1';
		elsif DATA_RDATA = X"5D" then		--elsif (c == ']' CNT <- CNT - 1
			CNT_dec <= '1';
		end if;
		PC_inc <= '1';				--PC <- PC + 1
	when While_end => 
		if DATA_RDATA = X"00" then		--if (mem[PTR] == 0)
		PC_inc <= '1';				--PC <- PC + 1
		NSTATE <= Fetch;
		else
		CNT_1 <= '1';				--CNT <- 1
		PC_dec <= '1';				--PC <- PC - 1
		DATA_EN <= '1';
		NSTATE <= While_end_skip;
		end if;
	when While_end_skip =>
		if CNT = X"00" then NSTATE <= Fetch;	--while (CNT != 0)
                else NSTATE <= While_end_loop;		
			DATA_EN <= '1';			--c <- mem[PC]
                end if;
	when While_end_loop =>
		if DATA_RDATA = X"5D" then 		--if (c == ']') CNT <- CNT + 1
			CNT_inc <= '1';
		elsif DATA_RDATA = X"5B" then		-- elsif (c == '[') CNT <- CNT - 1
			CNT_dec <= '1';
		end if;
		NSTATE <= While_end_exit;
	when While_end_exit =>
		if CNT = X"00" then PC_inc <= '1';	--if (CNT == 0) PC <- PC + 1 else PC <- PC - 1
		else PC_dec <= '1';
		end if;
		NSTATE <= While_end_skip;
	when Break => NSTATE <= Break_loop;		--c <- mem[PC]
		DATA_EN <= '1';
	when Break_loop =>
		if DATA_RDATA = X"5D" then		--if (c == ']') CNT <- CNT - 1
		NSTATE <= Fetch;
		CNT_dec <= '1';
		else NSTATE <= Break;
		end if;
		PC_inc <= '1';				--PC <- PC + 1
	end case;
end process;
end behavioral;

