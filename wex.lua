-- Quick löve wav exporter
-- zorg @ 2022 § ISC

-- Requires: Löve 11.4

-- Usage examples:
--[[
	local export_wav = require('wex')

	-- Option A: SoundData
	local SD
	SD = love.sound.newSoundData('test.wav')      -- Load audio from storage.
	SD = love.sound.newSoundData(1024,44100,16,2) -- Generate audio yourself.
	local flags = {} -- Optional export settings.
	love.filesystem.write('export_A.wav', export_wav(flags, SD))

	-- Option B: ByteData
	-- A contrived example; better one could be made from an ImageData...
	local BD = love.data.newByteData(love.sound.newSoundData('test.wav'))
	love.filesystem.write('export_B.wav', export_wav(nil, BD, 44100, 16, 2))

	-- Option C: Lua table
	local T = {}
	-- A simple sine wave.
	for i=0,1024 do T[i+1] = math.sin(i*2*math.pi*440.00/44100) end
	-- Lua tables have a bit depth of 64, since they are double precision floats; we export to 16bit int here though.
	love.filesystem.write('export_C.wav', export_wav({bitdepth = 16}, T, 44100, 64, 1))
--]]

-- data:
--[[
	-- SoundData            -> Format read from the SD object itself; source object only supports 8 & 16 bit depths.
	-- ByteData             -> Format given as additional parameters; source object only supports 8 & 16 bit depths.
	-- table of lua numbers -> Format given as additional parameters; the only source format that can be
	                           treated as a double-precision floating point. Uses 1-based indexing!
--]]

-- flags:
--[[
	- bitdepth     :number - can be: 8,16,24,32 for integral types, 32,64 for single or double precision float types;
	                         default is source format.
	- channelcount :number - can be 1 or 2;
	                         default is source format.
	- dither       :bool   - if true, for integer export, the lowest bit is set to a continuous 0-1 pattern;
	                         default is false.
	- fp           :bool   - if true, exports as floating-point, otherwise as integers; 
	                         default is false.
--]]

-- Version history:
--[[
	v0:
		- Initial release
	
	v1:
		- Fix odd-length padding.
		- Fix and separate out dithering algorithm.
	TODO:
		- Optimize for speed.
--]]

-- References used:
--[[
	- https://wavefilegem.com/how_wave_files_work.html
	- https://www.mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html
	- https://stackoverflow.com/questions/15576798/create-32bit-float-wav-file-in-python
	- https://web.archive.org/web/20080113195252/http://www.borg.com/~jglatt/tech/wave.htm
--]]

local ffi = require 'ffi'
if not ffi then error "This library requires löve to be built with LuaJIT." end



local dither = function(smp, state, limit)
	local LSb = smp % 2
	if     (LSb < state) then
		smp = math.min(smp + 1,  limit - 1)
	elseif (LSb > state) then
		smp = math.max(smp - 1, -limit    )
	end
	return smp
end



local wex = {}



wex.export = function(flags, data, samplerate, bitdepth, channelcount)

	if type(data) == 'userdata' and data.type and data:type() == 'SoundData' then
		samplerate    = data:getSampleRate()
		bitdepth      = data:getBitDepth()
		channelcount  = data:getChannelCount()
	end

	assert(type(samplerate)   == 'number')
	assert(type(bitdepth)     == 'number')
	assert(type(channelcount) == 'number')

	local smpCount
	if     type(data) == 'table' then
		smpCount      = #data / channelcount
	elseif type(data) == 'userdata' and data.type and data:type() == 'SoundData' then
		smpCount      = data:getSampleCount()
	elseif type(data) == 'userdata' and data.type and data:type() == 'ByteData'  then
		smpCount      = data:getSize() / channelcount / bitdepth/8
	end

	assert(type(smpCount) == 'number')

	if not flags              then flags              = {}           end
	if not flags.bitdepth     then flags.bitdepth     = bitdepth     end
	if not flags.channelcount then flags.channelcount = channelcount end
	if not flags.dither       then flags.dither       = false        end
	if not flags.fp           then flags.fp           = false        end

	-- Sanity check.
	if bitdepth == 64 and flags.bitdepth == 64 then flags.fp = true end



	-- Output byte payload.
	local bytes = smpCount * flags.bitdepth/8 * flags.channelcount



	local t = {}

	--[[ RIFF Header ]]--

	-- ChunkID: The string "RIFF". "RIFX"/"FFIR" big-endian format not supported.
	table.insert(t, love.data.pack("string", ">I4", 0x52494646))

	-- ChunkSize: Count of all bytes after this value in the file.
	if flags.fp then
		table.insert(t, love.data.pack("string", "<I4", 4 + (8 + 18) + (8 + 4) + (8 + bytes)))          
	else
		table.insert(t, love.data.pack("string", "<I4", 4 + (8 + 16) + (8 + bytes)))
	end

	-- Format: The string "WAVE".
	table.insert(t, love.data.pack("string", ">I4", 0x57415645))



	--[[ fmt (format) chunk ]]--

	-- chunkID:   The string "fmt ".
	table.insert(t, love.data.pack("string", ">I4", 0x666d7420))

	-- chunkSize: 16 for PCM, 18 for IEEE floating point.
	if flags.fp then
		table.insert(t, love.data.pack("string", "<I4", 18))
	else
		table.insert(t, love.data.pack("string", "<I4", 16))
	end

	-- AudioFormat: 1 for linear PCM. (2 for ADPCM, not supported.) 3 for IEEE floating point.
	if flags.fp then
		table.insert(t, love.data.pack("string", "<I2", 3))
	else
		table.insert(t, love.data.pack("string", "<I2", 1))
	end

	-- NumChannels:
	table.insert(t, love.data.pack("string", "<I2", flags.channelcount))
	-- SampleRate:
	table.insert(t, love.data.pack("string", "<I4", samplerate))
	-- ByteRate:
	table.insert(t, love.data.pack("string", "<I4", samplerate * flags.channelcount * flags.bitdepth/8))
	-- BlockAlign:
	table.insert(t, love.data.pack("string", "<I2",              flags.channelcount * flags.bitdepth/8))
	-- BitsPerSample:
	table.insert(t, love.data.pack("string", "<I2",                                   flags.bitdepth))

	-- ExtensionSize: Not defined for PCM, size of 0 for IEEE floats.
	if flags.fp then
		table.insert(t, love.data.pack("string", "<I2", 0))                                      
	end



	--[[ fact chunk ]]--

	-- Only necessary for non-PCM files.
	if flags.fp then
		-- chunkID: The string "fact".
		table.insert(t, love.data.pack("string", ">I4", 0x66616374))
		-- chunkSize: Size of the rest of this chunk.
		table.insert(t, love.data.pack("string", "<I4",          4))
		-- sampleFrames: How many simultaneously played back samplepoints exist in the file (channels are simultaneous).
		table.insert(t, love.data.pack("string", "<I4",   smpCount))
	end



	--[[ data chunk ]]--

	-- chunkID: The string "data".
	table.insert(t, love.data.pack("string", ">I4", 0x64617461))
	-- chunkSize: Size of the rest of this chunk; Raw data payload size, in bytes.
	table.insert(t, love.data.pack("string", "<I4", bytes))



	t = table.concat(t)

	-- If a chunk body has an odd number of bytes, it must be followed by a padding byte with value 0. 
	-- In other words, a chunk must always occupy an even number of bytes in the file.
	-- The padding byte should not be counted in the chunk header’s size field.
	-- For example, if a chunk body is 17 bytes in size, the header’s size field should be set to 17,
	-- even though the chunk body occupies 18 bytes (17 bytes of data followed by the padding byte).
	--
	-- The only chunk this can happen to is the data chunk, for the formats this library supports,
	-- and the fix is just inserting an extra 0 at the end.
	-- And even then, only when one's using 8bit integer bit depth with one channel, as the final export format.
	local padding = (bytes % 2 == 1)



	-- Let's reserve a ByteData with enough space for the output format we want.
	local BD = love.data.newByteData(#t + bytes + (padding and 1 or 0))
	
	-- Copy over the header.
	local n = #t
	t = love.data.newByteData(t)
	ffi.copy(BD:getFFIPointer(), t:getFFIPointer(), n)
	t:release()

	-- Treat the same ByteData object as if it only held the raw data.
	local DW = love.data.newDataView(BD, n, bytes)
	local dst_ptr = ffi.cast('uint8_t*', DW:getFFIPointer())

	local src_ptr = type(data) == 'table' and data or
		ffi.cast(bitdepth == 8 and 'uint8_t*' or 'int16_t*', data:getFFIPointer())

	-- Lua table indexing fix.
	local src_ofs = type(data) == 'table' and 1 or 0

	local dither_state = 0

	local src_ctr = 0
	local dst_ctr = 0

	while dst_ctr < bytes do

		if flags.channelcount == 1 then

			-- Read in a samplepoint
			local smp

			if channelcount == 1 then
				smp     = src_ptr[src_ctr + src_ofs]
				src_ctr = src_ctr + 1
			else
				-- Extract mid channel.
				smp     = (src_ptr[src_ctr + src_ofs] + src_ptr[src_ctr + src_ofs + 1]) / 2.0
				src_ctr = src_ctr + 2
			end

			-- Convert it to the [-1,1] range, into a lua number, which is a double.
			if     bitdepth ==  8 then smp = (smp - 127) / 2^ 7 -- unsigned
			elseif bitdepth == 16 then smp =  smp        / 2^15
			elseif bitdepth == 24 then smp =  smp        / 2^23 -- useable only with table input.
			elseif bitdepth == 32 then smp =  smp        / 2^31 -- useable only with table input. Assume integer.
			end

			-- Convert it to the wanted output range
			if not flags.fp then
				if     flags.bitdepth ==  8 then

					smp = smp * 2^ 7 + 2^ 7-1 -- unsigned
					if flags.dither then smp = dither(smp, dither_state, 2^ 7) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<I1', smp):getFFIPointer(), 1)
					dst_ctr = dst_ctr + 1

				elseif flags.bitdepth == 16 then

					smp = smp * 2^15
					if flags.dither then smp = dither(smp, dither_state, 2^15) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<i2', smp):getFFIPointer(), 2)
					dst_ctr = dst_ctr + 2

				elseif flags.bitdepth == 24 then

					smp = smp * 2^23
					if flags.dither then smp = dither(smp, dither_state, 2^23) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<i3', smp):getFFIPointer(), 3)
					dst_ctr = dst_ctr + 3

				elseif flags.bitdepth == 32 then

					smp = smp * 2^31
					if flags.dither then smp = dither(smp, dither_state, 2^31) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<i4', smp):getFFIPointer(), 4)
					dst_ctr = dst_ctr + 4

				end

				if flags.dither then dither_state = (dither_state + 1) % 2 end

			else
				if     flags.bitdepth == 32 then

					-- Just treat as a float.
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<f', smp):getFFIPointer(), 4)
					dst_ctr = dst_ctr + 4

				elseif flags.bitdepth == 64 then

					-- Nothing to do, lua numbers are doubles in löve.
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<d', smp):getFFIPointer(), 8)
					dst_ctr = dst_ctr + 8

				end
			end

		elseif flags.channelcount == 2 then

			-- Read in a samplepoint pair
			local smpL, smpR

			if channelcount == 1 then
				-- Duplicate.
				smpL = src_ptr[src_ctr + src_ofs]
				smpR = src_ptr[src_ctr + src_ofs]
				src_ctr = src_ctr + 1
			else
				smpL = src_ptr[src_ctr + src_ofs]
				smpR = src_ptr[src_ctr + src_ofs + 1]
				src_ctr = src_ctr + 2
			end

			-- Convert them to the [-1,1] range, into lua numbers, which are doubles.
			if     bitdepth ==  8 then
				smpL = (smpL - 127) / 2^ 7 -- unsigned
				smpR = (smpR - 127) / 2^ 7 -- unsigned
			elseif bitdepth == 16 then
				smpL =  smpL        / 2^15
				smpR =  smpR        / 2^15
			elseif bitdepth == 24 then
				smpL =  smpL        / 2^23 -- useable only with table input.
				smpR =  smpR        / 2^23 -- useable only with table input.
			elseif bitdepth == 32 then
				smpL =  smpL        / 2^31 -- useable only with table input. Assume integer.
				smpR =  smpR        / 2^31 -- useable only with table input. Assume integer.
			end

			-- Convert them to the wanted output range
			if not flags.fp then
				if     flags.bitdepth ==  8 then

					smpL = smpL * 2^ 7 + 127 -- unsigned
					if flags.dither then smpL = dither(smpL, dither_state, 2^ 7) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<I1', smpL):getFFIPointer(), 1)
					dst_ctr = dst_ctr + 1

					smpR = smpR * 2^ 7 + 127 -- unsigned
					if flags.dither then smpR = dither(smpR, dither_state, 2^ 7) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<I1', smpR):getFFIPointer(), 1)
					dst_ctr = dst_ctr + 1

				elseif flags.bitdepth == 16 then

					smpL = smpL * 2^15
					if flags.dither then smpL = dither(smpL, dither_state, 2^15) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<i2', smpL):getFFIPointer(), 2)
					dst_ctr = dst_ctr + 2

					smpR = smpR * 2^15
					if flags.dither then smpR = dither(smpR, dither_state, 2^15) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<i2', smpR):getFFIPointer(), 2)
					dst_ctr = dst_ctr + 2

				elseif flags.bitdepth == 24 then

					smpL = smpL * 2^23
					if flags.dither then smpL = dither(smpL, dither_state, 2^23) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<i3', smpL):getFFIPointer(), 3)
					dst_ctr = dst_ctr + 3

					smpR = smpR * 2^23
					if flags.dither then smpR = dither(smpR, dither_state, 2^23) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<i3', smpR):getFFIPointer(), 3)
					dst_ctr = dst_ctr + 3

				elseif flags.bitdepth == 32 then

					smpL = smpL * 2^31
					if flags.dither then smpL = dither(smpL, dither_state, 2^31) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<i4', smpL):getFFIPointer(), 4)
					dst_ctr = dst_ctr + 4

					smpR = smpR * 2^31
					if flags.dither then smpR = dither(smpR, dither_state, 2^31) end
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<i4', smpR):getFFIPointer(), 4)
					dst_ctr = dst_ctr + 4

				end

				if flags.dither then dither_state = (dither_state + 1) % 2 end

			else
				if     flags.bitdepth == 32 then

					-- Just treat as a float.
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<f', smpL):getFFIPointer(), 4)
					dst_ctr = dst_ctr + 4

					-- Just treat as a float.
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<f', smpR):getFFIPointer(), 4)
					dst_ctr = dst_ctr + 4

				elseif flags.bitdepth == 64 then

					-- Nothing to do, lua numbers are doubles in löve.
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<d', smpL):getFFIPointer(), 8)
					dst_ctr = dst_ctr + 8

					-- Nothing to do, lua numbers are doubles in löve.
					ffi.copy(dst_ptr + dst_ctr, love.data.pack('data', '<d', smpR):getFFIPointer(), 8)
					dst_ctr = dst_ctr + 8

				end
			end

		end
	end
	
	if padding then
		-- Write an extra zero byte, per spec.
		ffi.fill(dst_ptr + bytes, 1, 0x00)
	end
	
	return BD
end

return wex.export
