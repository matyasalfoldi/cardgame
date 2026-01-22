#pragma once
#include <string>
#include <vector>
#include <fstream>
#include <sstream>


struct CardModel {
    std::string imageFile;
    std::string caption;
    int atk;

    CardModel()
    {
        imageFile = "";
        caption = "Nothing";
        atk = -1;
    }
};

struct AppModel {
    bool inGame = false;
    bool game_finished = false;
    int turn;
    int player_points;
    int npc_points;
    CardModel card;
    CardModel player_deck_count_card;
    CardModel npc_deck_count_card;
    std::vector<CardModel> player_cards;
    std::vector<CardModel> npc_cards;
    CardModel player_chosen;
    CardModel npc_chosen;
    std::vector<CardModel> player_deck;
    std::vector<CardModel> npc_deck;
    

    AppModel()
        : turn(0), player_points(0), npc_points(0)
    {
        card.caption = "Card";
        card.imageFile = "example.png";
        player_cards.resize(3);
        npc_cards.resize(3);

        player_chosen.caption = "Nothing";
        player_chosen.imageFile = "example.png";
        npc_chosen.caption = "Nothing";
        npc_chosen.imageFile = "example.png";

        for (auto& slot : player_cards)
        {
            slot.caption = "Nothing";
        }
        for (auto& slot : npc_cards)
        {
            slot.caption = "Nothing";
        }

        player_deck = load_cards_from_file("player.txt");
        npc_deck = load_cards_from_file("npc.txt");
    }

    void fill_player_and_npc_deck_count()
    {
        player_deck_count_card.imageFile = "example.png";
        player_deck_count_card.caption = std::to_string(player_deck_count());
        npc_deck_count_card.imageFile = "example.png";
        npc_deck_count_card.caption = std::to_string(npc_deck_count());
    }

    int player_deck_count()
    {
        return player_deck.size();
    }

    int npc_deck_count()
    {
        return npc_deck.size();
    }

    void calculate_turn()
    {
        auto it = std::find_if(npc_cards.begin(), npc_cards.end(),
            [](const CardModel& c)
            {
                return c.caption != "Nothing"; }
        );
        size_t npc_idx = std::distance(npc_cards.begin(), it);
        npc_chosen.caption = "Nothing";
        std::swap(npc_cards[npc_idx], npc_chosen);

        if (player_chosen.atk > npc_chosen.atk)
        {
            ++player_points;
        }
        else if (player_chosen.atk == npc_chosen.atk)
        {
            ++player_points;
            ++npc_points;
        }
        else
        {
            ++npc_points;
        }
        ++turn;
    }

    void draw_card()
    {
        if (player_deck.empty())
        {
            bool all_empty = true;
            for (auto c : player_cards)
            {
                if (c.caption != "Nothing")
                {
                    all_empty = false;
                    break;
                }
            }
            if (all_empty)
            {
                game_finished = true;
            }
            return;
        }
        for (auto& slot : player_cards)
        {
            if (slot.caption == "Nothing")
            {
                slot = player_deck.back();
                player_deck.pop_back();
                player_deck_count_card.caption = std::to_string(player_deck_count());
                break;
            }
        }
        for (auto& slot : npc_cards)
        {
            if (slot.caption == "Nothing")
            {
                slot = npc_deck.back();
                npc_deck.pop_back();
                npc_deck_count_card.caption = std::to_string(npc_deck_count());
                return;
            }
        }
    }

    std::vector<CardModel> load_cards_from_file(const std::string& filename)
    {
        std::vector<CardModel> cards;
        std::ifstream file(filename);

        if (!file.is_open()) {
            throw std::runtime_error("Could not open file: " + filename);
        }

        std::string line;
        while (std::getline(file, line)) {
            std::stringstream ss(line);
            CardModel card;

            ss >> card.imageFile >> card.caption >> card.atk;

            if (!ss.fail()) {
                cards.push_back(card);
            }
        }

        return cards;
    }
};
