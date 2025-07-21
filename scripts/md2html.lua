function Math(elem)
    if elem.mathtype == 'DisplayMath' then
        -- Replace display math delimiters
        return pandoc.RawInline('html', '\\[' .. elem.text .. '\\]')
    elseif elem.mathtype == 'InlineMath' then
        -- Replace inline math delimiters
        return pandoc.RawInline('html', '\\(' .. elem.text .. '\\)')
    end
end

function Para(elem)
    -- Append CUSTOM_BREAK after each paragraph
    return {pandoc.Plain(elem.content), pandoc.RawInline('html', 'CUSTOM_BREAK')}
end

function Pandoc(doc)
    -- Check the last block of the document and remove a trailing CUSTOM_BREAK if present
    if #doc.blocks > 0 then
        local lastBlock = doc.blocks[#doc.blocks]

        -- The last block may be a Plain or Para depending on transformations
        if (lastBlock.t == "Plain" or lastBlock.t == "Para") and #lastBlock.content > 0 then
            local lastInline = lastBlock.content[#lastBlock.content]
            if lastInline.t == "RawInline" and lastInline.text == "CUSTOM_BREAK" then
                table.remove(lastBlock.content, #lastBlock.content)
            end
        end
    end
    return doc
end
