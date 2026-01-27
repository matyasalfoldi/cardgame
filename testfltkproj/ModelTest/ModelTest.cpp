#include "pch.h"
#include "CppUnitTest.h"
#include "Model.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace AppModelTests
{
    TEST_CLASS(AppModelInitializationTests)
    {
    public:

        TEST_METHOD(Constructor_InitializesDefaults)
        {
            // Arrange
            // We must avoid file loading errors, so wrap in try/catch.
            AppModel model;

            // Act / Assert
            Assert::AreEqual(0, model.turn);
            Assert::AreEqual(0, model.player_points);
            Assert::AreEqual(0, model.npc_points);

            Assert::AreEqual(std::string("Card"), model.card.caption);
            Assert::AreEqual(std::string("example.png"), model.card.imageFile);

            Assert::AreEqual(size_t(3), model.player_cards.size());
            Assert::AreEqual(size_t(3), model.npc_cards.size());

            for (auto& c : model.player_cards)
                Assert::AreEqual(std::string("Nothing"), c.caption);

            for (auto& c : model.npc_cards)
                Assert::AreEqual(std::string("Nothing"), c.caption);
        }
    };


    TEST_CLASS(AppModelDeckTests)
    {
    public:

        TEST_METHOD(DeckCount_ReturnsCorrectValues)
        {
            AppModel model;

            // Override decks to avoid file dependency
            model.player_deck = { CardModel(), CardModel(), CardModel() };
            model.npc_deck = { CardModel(), CardModel() };

            Assert::AreEqual(3, model.player_deck_count());
            Assert::AreEqual(2, model.npc_deck_count());
        }

        TEST_METHOD(FillDeckCount_UpdatesCaption)
        {
            AppModel model;

            model.player_deck = { CardModel(), CardModel() };
            model.npc_deck = { CardModel() };

            model.fill_player_and_npc_deck_count();

            Assert::AreEqual(std::string("2"), model.player_deck_count_card.caption);
            Assert::AreEqual(std::string("1"), model.npc_deck_count_card.caption);
        }
    };


    TEST_CLASS(AppModelDrawCardTests)
    {
    public:

        TEST_METHOD(DrawCard_MovesCardIntoPlayerSlot)
        {
            AppModel model;

            // Prepare decks
            CardModel c1; c1.caption = "A"; c1.atk = 5;
            CardModel c2; c2.caption = "B"; c2.atk = 3;

            model.player_deck = { c1 };
            model.npc_deck = { c2 };

            // Ensure slots are empty
            for (auto& c : model.player_cards) c.caption = "Nothing";
            for (auto& c : model.npc_cards) c.caption = "Nothing";

            model.draw_card();

            // Player should receive card A
            Assert::AreEqual(std::string("A"), model.player_cards[0].caption);

            // NPC should receive card B
            Assert::AreEqual(std::string("B"), model.npc_cards[0].caption);

            // Decks should now be empty
            Assert::AreEqual(0, model.player_deck_count());
            Assert::AreEqual(0, model.npc_deck_count());
        }

        TEST_METHOD(DrawCard_EmptyDeckAndEmptySlots_FinishesGame)
        {
            AppModel model;

            model.player_deck.clear();
            for (auto& c : model.player_cards) c.caption = "Nothing";

            model.draw_card();

            Assert::IsTrue(model.game_finished);
        }
    };


    TEST_CLASS(AppModelTurnCalculationTests)
    {
    public:

        TEST_METHOD(CalculateTurn_PlayerWins)
        {
            AppModel model;

            // Set chosen cards
            model.player_chosen.atk = 10;
            model.npc_chosen.atk = 5;

            // NPC must have a non-"Nothing" card to swap out
            model.npc_cards[0].caption = "X";
            model.npc_cards[0].atk = 5;

            model.calculate_turn();

            Assert::AreEqual(1, model.player_points);
            Assert::AreEqual(0, model.npc_points);
            Assert::AreEqual(1, model.turn);
        }

        TEST_METHOD(CalculateTurn_Draw)
        {
            AppModel model;

            model.player_chosen.atk = 7;
            model.npc_cards[0].caption = "Y";
            model.npc_cards[0].atk = 7;

            model.calculate_turn();

            Assert::AreEqual(1, model.player_points);
            Assert::AreEqual(1, model.npc_points);
        }

        TEST_METHOD(CalculateTurn_PlayerLoses)
        {
            AppModel model;

            model.player_chosen.atk = 2;
            model.npc_cards[0].caption = "Z";
            model.npc_cards[0].atk = 9;

            model.calculate_turn();

            Assert::AreEqual(0, model.player_points);
            Assert::AreEqual(1, model.npc_points);
        }
    };
}
